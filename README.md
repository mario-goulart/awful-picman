awful-picman is a web-based picture management software in CHICKEN
Scheme.

**WARNING**: This is alpha software.  Everything (including the
internal database layout!) is subject to change without any concern
with regard to backward compatibility.


## Features

* i18n support
* Picture annotations (tags, date, description)
* Directory, album, filter and tag views
* Tiff support
* OCR support (for scanned documents in tiff format)

## Requirements

* [CHICKEN](http://call-cc.org) & some eggs
* imlib2 development files (libimlib2-dev package on Debian & derivatives)
* libexif development files (libexif-dev package on Debian & derivatives)
* [Tesseract](https://code.google.com/p/tesseract-ocr/) (optional, for OCR support)

## Installing

    $ git clone https://github.com/mario-goulart/awful-picman.git
    $ cd awful-picman
    $ chicken-install
 
## Using

    $ cd dir-with-pictures
    $ awful-picman --init

Then point your favorite web browser to `http://localhost:8080`

See the output of `awful-picman --help` for some obscure options.

## Configuring

awful-picman may read some configuration files.  They are read in the
following order:

* `/etc/awful-picman.conf`
* `$HOME/.awful-picman.conf`
* `<pics dir>/.awful-picman/awful-picman.conf`

If they don't exist, awful-picman will just ignore them.  Files read
last clobber settings made by previous ones.

See `src/awful-picman-params.scm` for the available configuration
parameters.


## Obscure tips

### Removing picture files

awful-picman won't ever alter your picture files.  So, it's not
possible to use awful-picman to remove pictures you don't want, for
example.  However, you can use awful-picman to generate a list of
picture files you want to remove.  To do this, use the GUI to create
an album with the pictures you want to remove, then use the CLI
interface to list the album picture files.  Example:

    $ curl http://localhost:8080/raw/list-albums
    # Pick the album id (first column) of the album with the pictures to be removed

    $ curl http://localhost:8080/raw/album-pic-files/<album-id>
    # You'll get a list of filenames in the album of pictures to be removed

You can then use a tool like `rm` to remove the files.  After that you
can use the GUI to remove the album and run `awful-picman --gc` to
clean things up.
