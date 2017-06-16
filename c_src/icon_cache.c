#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <alloca.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdio.h>

#include <erl_nif.h>
#include <erl_driver.h>

#define UINT16(ptr, offset) ntohs(* (uint16_t*) (ptr + offset))
#define UINT32(ptr, offset) ntohl(* (uint32_t*) (ptr + offset))


static ErlNifResourceType* cache_type;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_invalid_cache_file;


typedef struct {
    void *buf;
    size_t size;

    uint32_t n_buckets;
    void* icon_offsets;
} cache_data;


static void cache_dtor(ErlNifEnv *env, void *obj)
{
    cache_data *data = (cache_data *) obj;
    if (data->buf != NULL) munmap(data->buf, data->size);
}

/* gtk/docs/iconcache.txt
Header:
2                     CARD16          MAJOR_VERSION   1
2                     CARD16          MINOR_VERSION   0
4                     CARD32          HASH_OFFSET
4                     CARD32          DIRECTORY_LIST_OFFSET

Hash:
4                       CARD32          N_BUCKETS
4*N_BUCKETS             CARD32          ICON_OFFSET

DirectoryList:
4                       CARD32          N_DIRECTORIES
4*N_DIRECTORIES         CARD32          DIRECTORY_OFFSET

Icon:
4                       CARD32          CHAIN_OFFSET
4                       CARD32          NAME_OFFSET
4                       CARD32          IMAGE_LIST_OFFSET

ImageList:
4                       CARD32          N_IMAGES
8*N_IMAGES              Image           IMAGES

Image:
2                       CARD16          DIRECTORY_INDEX
2                       ICON_FLAGS      FLAGS
4                       CARD32          IMAGE_DATA_OFFSET

ICON_FLAGS
HAS_SUFFIX_PNG  1
HAS_SUFFIX_XPM  2
HAS_SUFFIX_SVG  4
HAS_ICON_FILE   8

ImageData:
4                       CARD32          IMAGE_PIXEL_DATA_OFFSET
4                       CARD32          IMAGE_META_DATA_OFFSET

4                       CARD32          IMAGE_PIXEL_DATA_TYPE
4                       CARD32          IMAGE_PIXEL_DATA_LENGTH
N/A                     N/A             PIXEL_DATA

IMAGE_PIXEL_DATA_TYPE
0 GdkPixdata format

MetaData:
4                       CARD32          EMBEDDED_RECT_OFFSET
4                       CARD32          ATTACH_POINT_LIST_OFFSET
4                       CARD32          DISPLAY_NAME_LIST_OFFSET

EmbeddedRect:
2                       CARD16          X0
2                       CARD16          Y0
2                       CARD16          X1
2                       CARD16          Y1

AttachPointList:
4                       CARD32          N_ATTACH_POINTS
4*N_ATTACH_POINTS       AttachPoint

AttachPoint:
2                       CARD16          X
2                       CARD16          Y

DisplayNameList:
4                       CARD32          N_DISPLAY_NAMES
4*N_DISPLAY_NAMES       DisplayName

DisplayName:
4                       CARD32          DISPLAY_LANG_OFFSET
4                       CARD32          DISPLAY_NAME_OFFSET

Notes:

* All offsets are from in bytes from the beginning of the file

* Strings are zero-terminated

* Directories are stored as relative paths.

* All numbers are in network (big-endian) order. This is
  necessary because the data will be stored in arch-independent
  directories like /usr/share/icons or even in user's
  home directories.
*/

static bool init_buffer(cache_data* data, ErlNifEnv* env, ERL_NIF_TERM file_name)
{
    int error;

    unsigned int name_size;
    if (!enif_get_list_length(env, file_name, &name_size))
        return false;

    char *name = alloca(name_size+1);
    if (enif_get_string(env, file_name, name, name_size+1, ERL_NIF_LATIN1) < 1)
        return false;

    int fd = open(name, O_RDONLY);
    if (fd == -1) goto ERROR;

    struct stat st;
    if (fstat(fd, &st) < 0) goto ERROR;

    void *buf = mmap(NULL, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (buf == MAP_FAILED) goto ERROR;

    close(fd);

    data->buf = buf;
    data->size = st.st_size;

    return true;

ERROR:
    error = errno;
    if (fd != -1) close(fd);

    enif_raise_exception(env, enif_make_atom(env, erl_errno_id(error)));

    return false;
}


static bool init_icon_offsets(cache_data* data, ErlNifEnv* env)
{
    if (data->size < 8) goto INVALID_FILE;

    uint16_t maj_ver = UINT16(data->buf, 0);
    uint16_t min_ver = UINT16(data->buf, 2);

    if (maj_ver != 1 || min_ver != 0) goto INVALID_FILE;

    uint32_t hash_off = UINT32(data->buf, 4);
    if (data->size < (hash_off + 4)) goto INVALID_FILE;

    data->n_buckets = UINT32(data->buf, hash_off);
    if (data->size < (hash_off + 4 + data->n_buckets  * 4)) goto INVALID_FILE;

    data->icon_offsets = data->buf + hash_off + 4;

    return true;

INVALID_FILE:
    enif_raise_exception(env, atom_invalid_cache_file);
    return false;
}


static bool get_dir_idx(cache_data* data, uint32_t n_dirs, void* dir_offsets,
                        ErlNifEnv* env, ERL_NIF_TERM dir, ERL_NIF_TERM* result)
{
    unsigned int size;
    if (!enif_get_list_length(env, dir, &size))
        return false;

    char *name = alloca(size+1);
    if (enif_get_string(env, dir, name, size+1, ERL_NIF_LATIN1) < 1)
        return false;

    for (uint32_t i = 0; i < n_dirs; ++i) {
        uint32_t offset = UINT32(dir_offsets, 4*i);

        if (offset >= data->size) {
            enif_raise_exception(env, atom_invalid_cache_file);
            return false;
        }

        if (strcmp((const char*)(data->buf + offset), name) == 0) {
            *result = enif_make_int(env, i);
            return true;
        }
    }

    *result = atom_undefined;
    return true;
}


static bool make_dirs_idx(cache_data* data, ErlNifEnv* env, ERL_NIF_TERM dirs,
                          ERL_NIF_TERM* result)
{
    void* buf = data->buf;
    size_t size = data->size;

    if (size < 12) goto INVALID_FILE;

    uint32_t dir_list_off = UINT32(data->buf, 8);
    if (size < (dir_list_off + 4)) goto INVALID_FILE;

    uint32_t n_dirs = UINT32(data->buf, dir_list_off);
    if (size < (dir_list_off + 4 + n_dirs  * 4)) goto INVALID_FILE;

    void* dir_offsets = buf + dir_list_off + 4;

    unsigned int dirs_size;
    if (!enif_get_list_length(env, dirs, &dirs_size)) return false;

    ERL_NIF_TERM dir_idx = enif_make_list(env, 0);
    ERL_NIF_TERM dir;

    for (unsigned i = 0; i < dirs_size; ++i) {
        enif_get_list_cell(env, dirs, &dir, &dirs);

        ERL_NIF_TERM idx;
        if (!get_dir_idx(data, n_dirs, dir_offsets, env, dir, &idx)) return false;

        dir_idx = enif_make_list_cell(env, idx, dir_idx);
    }

    enif_make_reverse_list(env, dir_idx, result);

    return true;

INVALID_FILE:
    enif_raise_exception(env, atom_invalid_cache_file);
    return false;
}


static uint32_t name_hash(char const * name)
{
  uint32_t hash = *name;

  if (hash) {
    for (name += 1; *name != '\0'; name++)
      hash = (hash << 5) - hash + *name;
  }

  return hash;
}


static ERL_NIF_TERM image_list_data(cache_data* data, ErlNifEnv* env, uint32_t offset)
{
    uint32_t n_images = UINT32(data->buf, offset);

    void* image_list = data->buf + offset + 4;

    ERL_NIF_TERM result = enif_make_list(env, 0);

    for (unsigned i = 0; i < n_images; ++i) {
        void* item_data = image_list + 8 * i;

        uint16_t idx = UINT16(item_data, 0);
        uint16_t flags = UINT16(item_data, 2);
        uint32_t img_offset = UINT32(item_data, 4);

        ERL_NIF_TERM item = enif_make_tuple3(env,
                                             enif_make_int(env, idx),
                                             enif_make_int(env, flags),
                                             enif_make_int(env, img_offset));
        result = enif_make_list_cell(env, item, result);
    }

    return result;
}


static ERL_NIF_TERM
init_cache_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    cache_data *data = (cache_data *) enif_alloc_resource(cache_type,
                                                          sizeof(cache_data));
    data->buf = NULL;
    data->size = 0;

    if (!init_buffer(data, env, argv[0])) goto ERROR;
    if (!init_icon_offsets(data, env)) goto ERROR;

    ERL_NIF_TERM dir_idx;
    if (!make_dirs_idx(data, env, argv[1], &dir_idx)) goto ERROR;

    ERL_NIF_TERM cache = enif_make_resource(env, data);
    enif_release_resource(data);

    return enif_make_tuple2(env, cache, dir_idx);

ERROR:
    enif_release_resource(data);

    ERL_NIF_TERM reason;
    if (enif_has_pending_exception(env, &reason)) return reason;

    return enif_make_badarg(env);
}


static ERL_NIF_TERM
lookup_cache_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void *ptr;
    if (!enif_get_resource(env, argv[0], cache_type, &ptr))
        return enif_make_badarg(env);

    cache_data *data = (cache_data *) ptr;

    unsigned int id_size;
    if (!enif_get_list_length(env, argv[1], &id_size))
        return enif_make_badarg(env);

    char *id = alloca(id_size+1);
    if (enif_get_string(env, argv[1], id, id_size+1, ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    uint32_t hash = name_hash(id) % data->n_buckets;

    uint32_t offset = UINT32(data->icon_offsets, 4 * hash);

    while (offset != 0xffffffff) {
        uint32_t name_offset = UINT32(data->buf, offset + 4);
        const char *name = data->buf + name_offset;

        if (strcmp (name, id) == 0) {
            uint32_t image_list_offset = UINT32(data->buf, offset + 8);
            return image_list_data(data, env, image_list_offset);
        }
  
        offset = UINT32(data->buf, offset);
    }

    return atom_undefined;
}


static ErlNifFunc nifs[] =
{
    {"init_cache_nif",   2, init_cache_nif,   ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"lookup_cache_nif", 2, lookup_cache_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};


static void init_atoms(ErlNifEnv* env)
{
    atom_undefined = enif_make_atom(env, "undefined");
    atom_invalid_cache_file = enif_make_atom(env, "invalid_cache_file");
}


static ErlNifResourceType* open_resource_type(ErlNifEnv *env,
                                              ErlNifResourceFlags flags)
{
    return enif_open_resource_type(env, NULL, "icon_cache", cache_dtor, flags, NULL);
}


static int onload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    cache_type = open_resource_type(env, ERL_NIF_RT_CREATE);
    if (cache_type == NULL) return -1;

    init_atoms(env);

    return 0;
}


static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
                   ERL_NIF_TERM load_info)
{
    cache_type = open_resource_type(env, ERL_NIF_RT_TAKEOVER);
    if (cache_type == NULL) return -1;

    init_atoms(env);

    return 0;
}


ERL_NIF_INIT(themes_icon,nifs,onload,NULL,upgrade,NULL)
