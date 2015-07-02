pack(1) -- pack and unpack archives
===================================

## SYNOPSIS

`pack` [options] [files...]

`unpack` [options] [files...]

## DESCRIPTION

`pack` builds an archive from all the specified files.
The files’ common prefix is removed, except for the last entry, which is also taken as the default base for the filename.

`unpack` extracts files from archives with sane defaults.
It ensures that there is exactly one new file in the current directory,
with the basename of the archive.

## OPTIONS

### common

* `-f`, `--force`:
  Overwrite already existing files.

* `-h`, `--help`:
  Display help.

### pack

* `-n`, `--name` `name`:
  Use `name` as the basename for the archive instead of the last shared directory entry.

* `-t`, `--type`:
  Create archive of this type instead of `tar.xz`.

### unpack

* `-k`, `--keep`:
  Preserve the original archive.

* `-t`, `--type`:
  Extract from the outermost (matching) archives.

    `unpack -t gz hello.tar.gz`

  will yeild `hello.tar` not a directory named `hello`.

## FILE FORMATS

### pack and unpack
* 7zip: `.7z` 7z(1)
* archive: `.a` ar(1)
* arc: `.arc` arc(1)
* arj: `.arj` arj(1)
* bzip2: `.bz2` bzip2(1)
* cab: `.cab` lcab(1) cabextract(1)
* compress: `.Z` compress(1)
* gzip: `.gz` gzip(1)
* kgb: `.kgb` kgb(1)
* lrzip: `.lrz` lrzip(1)
* lzip: `.lz` lzip(1)
* lzma: `.lzma` lzma(1)
* lzop: `.lzo` lzop(1)
* rar: `.rar` rar(1) unrar(1)
* rzip: `.rz` rzip(1)
* tar: `.tar` tar(1)
* xz: `.xz` xz(1)
* zip: `.zip` zip(1) unzip(1)
* zoo: `.zoo` zoo(1)
* zpaq: `.zpaq` zpaq(1)

### unpack only
* ace: `.ace` unace(1)
* adf: `.adf` unadf(1)
* alz: `.alz` unalz(1)
* deb: `.deb` dpkg-deb(1)
* dms: `.dms` xdms(1)
* lha: `.lzh` `.lzs` lha(1)

## BUGS

* Helper programs are almost universally insecure.
* If the current directory is included in an archive,
  should the archive be placed the nearest non-contained parent directory?
* Important file formats to implement:
  cpio, rpm, ext2, hfs+ (dmg), fat32, iso, udf, various shar
* Magic detection does not work with some formats.
  At the very least: `.kgb`, `.lzo`, `.rz`, `.zpaq`, any `.exe`
* `.tgz`, `.tZ`, `.tbz2`, `.txz`, and so forth should be parsed as a list.
  As extensions they will to be trimmed too early and the file will probably not unpack all the way.
* `unpack` will not read a list of types, just one type.

## COPYRIGHT

Public Domain

## SEE ALSO

atool(1), patool(1)

