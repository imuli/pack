pack(1)
=======

## SYNOPSIS

`pack` [options] [files...]

`unpack` [options] [files...]

## DESCRIPTION

`pack` and `unpack` are archive and compression utilities with sane defaults.

`pack` creates an archive containing all the specified files, with a sensible name.

`unpack` extracts each file into a file (if it contains only one file) or directory (otherwise) with that name.

## OPTIONS

* `--`
	All following arguments are files.
* `-f`, `--format`
	Force use of this format.
* `-h`, `-help`, `--help`
	Display help.
* `-k`
	Keep original file.
* `-v`, `-verbose`
	Be noisy.

## BUGS

* Everything
* External programs are almost universally insecure.

## COPYRIGHT

Public Domain

## SEE ALSO

gzip(1), bzip2(1), xz(1), tar(1), unzip(1), zip(1)

