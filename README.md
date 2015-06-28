pack(1)
=======

## SYNOPSIS

`pack` [options] [files...]

`unpack` [options] [files...]

## DESCRIPTION

`pack` and `unpack` are archive and compression utilities with sane defaults.
`pack` compresses each specified file or directory into it's own archive.
`unpack` extracts each file into a file (if it contains only one file) or directory (otherwise) with that name.

## OPTIONS

* `--`
	All following arguments are files.
* `-h`, `-help`, `--help`
	Display help.
* `-k`
	Keep original file.

## BUGS

* Currently written in rc shell.
* External programs are almost universally insecure.

## COPYRIGHT

Public Domain

## SEE ALSO

gzip(1), bzip2(1), xz(1), tar(1), unzip(1), zip(1)

