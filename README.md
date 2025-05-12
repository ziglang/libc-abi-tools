# libc ABI tools

This repository contains a collection of tools to be used by Zig maintainers to
generate `abilists` files for various libc implementations; Zig users have no
direct use for this repository.

An `abilists` file can be used by the Zig compiler to generate an accurate stub
shared library when dynamically linking libc in cross-compilation scenarios. The
generated `abilists` files are shipped with the Zig compiler, so an effort is
made to keep them compact.

Currently supported libcs:

* FreeBSD libc
* Linux glibc
* NetBSD libc

## `abilists` binary format

All integers are stored little-endian.

- `u8` number of libc libraries. For each:
  - null-terminated name, e.g. `"c\x00"`, `"m\x00"`, `"dl\x00"`, `"pthread\x00"`
- `u8` number of libc versions, sorted ascending. For each:
  - `u8` major
  - `u8` minor
  - `u8` patch
- `u8` number of targets. For each:
  - null-terminated target string, e.g. `"arm-linux-gnueabi\x00"`
- `u16` number of function inclusions
  - null-terminated symbol name (not repeated for subsequent same symbol inclusions)
  - Set of Function Inclusions
- `u16` number of object inclusions
  - null-terminated symbol name (not repeated for subsequent same symbol inclusions)
  - Set of Object Inclusions
- `u16` number of TLS inclusions
  - null-terminated symbol name (not repeated for subsequent same symbol inclusions)
  - Set of TLS Inclusions

Set of Function Inclusions:
  - uleb128 (`u64`) set of targets this inclusion applies to (`1 << INDEX_IN_TARGET_LIST`)
  - `u8` index of libc library this inclusion applies to
    - unversioned is indicated if `1 << 5` bit is set in library index
    - weak linkage is indicated if `1 << 6` bit is set in library index
    - last inclusion is indicated if `1 << 7` bit is set in library index
  - `[N]u8` set of libc versions this inclusion applies to
    - last version index is indicated if `1 << 7` bit is set in version index

Set of Object Inclusions:
  - uleb128 (`u64`) set of targets this inclusion applies to (`1 << INDEX_IN_TARGET_LIST`)
  - uleb128 (`u16`) object size
  - `u8` index of libc library this inclusion applies to
    - unversioned is indicated if `1 << 5` bit is set in library index
    - weak linkage is indicated if `1 << 6` bit is set in library index
    - last inclusion is indicated if `1 << 7` bit is set in library index
  - `[N]u8` set of libc versions this inclusion applies to
    - last version index is indicated if `1 << 7` bit is set in version index

Set of TLS Inclusions:
  - uleb128 (`u64`) set of targets this inclusion applies to (`1 << INDEX_IN_TARGET_LIST`)
  - uleb128 (`u16`) object size
  - `u8` index of libc library this inclusion applies to
    - unversioned is indicated if `1 << 5` bit is set in library index
    - weak linkage is indicated if `1 << 6` bit is set in library index
    - last inclusion is indicated if `1 << 7` bit is set in library index
  - `[N]u8` set of libc versions this inclusion applies to
    - last version index is indicated if `1 << 7` bit is set in version index

## Debugging an `abilists` file

```sh
zig run list.zig -- abilists
```
