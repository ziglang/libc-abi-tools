# glibc ABI tools

This directory contains `.abilist` files from every version of glibc. These
files are consolidated to generate a single 247 KB symbol mapping file that is
shipped with Zig to target any version of glibc.

## Adding `.abilist` files for a new glibc version

1. Clone glibc

```sh
git clone git://sourceware.org/git/glibc.git
```

2. Check out the new glibc version git tag, e.g. `glibc-2.39`.

3. Run the tool to grab the new `.abilist` files:

   ```sh
   zig run collect.zig -- $GLIBC_GIT_REPO_PATH
   ```

4. This mirrors the directory structure into this directory,
   namespaced under the version number, but only copying files with the
   `.abilist` extension.

5. Inspect the changes and then commit these new files into git.

## Updating Zig

1. Add the new glibc versions to the `versions` global constant.

2. Make sure that `lib_names`, `zig_targets`, and `abi_lists` are up to date.

3. Run `consolidate.zig` in this directory:

   ```sh
   zig run consolidate.zig
   ```

This will generate the file `abilists` which you can then inspect and make sure
it is OK. Copy it to `$ZIG_GIT_REPO_PATH/lib/libc/glibc/abilists`.

## Strategy

The `.abilist` files from the latest glibc are *almost* enough to completely
encode all the information that we need to generate the symbols database. The
only problem is when a function migrates from one library to another. For
example, in glibc 2.32, the function `pthread_sigmask` migrated from libpthread
to libc, and the latest `.abilist` files only show it in libc. However, if a
user targets glibc 2.31, Zig needs to know to put the symbol into
`libpthread.so` and not `libc.so`.

In glibc upstream, they simply renamed the `.abilist` files from
`pthread.abilist`to `libc.abilist`. This resulted in the following line being
present in `libc.abilist` in glibc 2.32 and later:

```
GLIBC_2.0 pthread_sigmask F
```

This implies that in glibc 2.0, `libc.so` has the `pthread_sigmask` symbol,
which is incorrect, because it was only found in `libpthread.so`.

This is why this directory contains `.abilist` files from all past versions of
glibc as well as the most recent one - it allows us to detect this situation,
and generate a corrected symbols database.

The strategy is to start with the earliest glibc version, consume the `.abilist`
files, and then treat that data as correct. Then we move on to the next earliest
glibc version, but now we have to detect a contradiction: if the newer glibc
version claims that e.g. `pthread_sigmask` is available in glibc 2.0, when our
correct data says that it is not, we ignore that incorrect piece of data.
However, we must take in new data if the version it talks about is greater than
the version corresponding to the "correct" data set.

After merging in the newer glibc version, we mark the current dataset as
"correct" and move on to the next, and so on until we have processed all the
sets of `.abilist` files.

When this process completes, we have in memory something that looks like this:

* For each glibc symbol
  * For each glibc library
    * For each target
      * For each glibc version
        * Whether the symbol is absent, a function, or an object+size

And our job is now to *encode* this information into a file that does not waste
installation size and yet remains simple to decode and use in the Zig compiler.

### Inclusions

Next, the script generates the minimal number of "inclusions" to encode all the
information. An "inclusion" is:

 * A symbol name.
 * The set of targets this inclusion applies to.
 * The set of glibc versions this inclusion applies to.
 * The set of libraries this inclusion applies to.
 * Whether it is a function or object, and if an object, its size in bytes.

As an example, consider `dlopen`. An inclusion is something like this:

 * `dlopen`
 * targets: `aarch64-linux-gnu` `powerpc64le-linux-gnu`
 * versions: 2.17 2.34
 * libraries: `libdl.so`
 * type: function

This does not cover all the places `dlopen` can be found however. There will
need to be more inclusions for more targets, for example:

 * `dlopen`
 * targets: `x86_64-linux-gnu`
 * versions: 2.2.5 2.34
 * libraries: `libdl.so`
 * type: function

Now we have more coverage of all the places `dlopen` can be found, but there are
yet more that need to be emitted. The script emits as many inclusions as
necessary so that all the information is represented.

Next we make a few observations which lead to a more compact data encoding.

### Observation: All symbols are consistently either functions or objects

There is no symbol that is a function on one target, and an object on another
target. Similarly there is no symbol that is a function on one glibc version,
but an object in another, and there is no symbol that is a function in one
shared library, but an object in another.

We exploit this by encoding functions and object symbols in separate lists.

### Observation: Over half of the objects are exactly 4 bytes

51% of all object entries are 4 bytes, and 68% of all object entries are either
4 or 8 bytes.

Total object inclusions are 765. If we stored 4 and 8 byte objects in separate
lists, this would save 2 bytes from 520 inclusions, totaling 1 KB. Not worth.

### Observation: Average number of different versions per inclusion is 1.02

Nearly every inclusion has typically 1 version attached to it, rarely more.
This makes a `u64` bitset uneconomical. With 19530 total inclusions, this comes
out to 153 KB spent on the version bitset. However if we encoded it as one byte
per version, using 1 bit of the byte to indicate the terminal item, this would
bring the 153 KB down to 19 KB. That is almost a 50% reduction from the total
size of the encoded `abilists` file. Definitely worth it.
