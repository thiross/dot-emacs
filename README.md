Zale's emacs configure files.
=============================

WSLg / HiDPI
------------

Use an Emacs build configured with `--with-pgtk` for native Wayland rendering.
X11 windows can look blurry when WSLg scales them on a HiDPI display.
Keep WSLg's HiDPI scaling enabled and let GTK follow the monitor scale.
The Linux default font is Maple Mono at 10 points; the floating-point font size
in `lisp/custom-packages.el` specifies points rather than fixed pixels.

Launch a PGTK build with `GDK_BACKEND=wayland` under WSLg. Evaluate
`window-system` with `M-:` to check the display backend; it should be `pgtk`.

For WSLg, use this shell setup instead of overriding `DISPLAY` to the Windows
host or forcing `GDK_SCALE`:

```sh
export GDK_BACKEND=wayland
unset GDK_SCALE
export XCURSOR_THEME=Bibata-Modern-Classic
export XCURSOR_SIZE=12
```

A build matching the current global installation can be prepared with:

```sh
mkdir -p /tmp/emacs-pgtk-build
cd /tmp/emacs-pgtk-build
/path/to/emacs-source/configure \
  --prefix=/usr/local \
  --with-pgtk \
  --with-native-compilation=aot \
  --with-tree-sitter \
  --with-imagemagick \
  --with-cairo \
  --with-harfbuzz \
  --with-sqlite3 \
  --with-modules \
  --with-dbus \
  --with-file-notification=inotify
make -j"$(nproc)"
```

`make uninstall` only works while the build tree still exists, so do not keep the
build only under `/tmp` if installing with `make`.

To install directly from the build tree:

```sh
sudo make install
```

Alternatively, package the staged installation as a local `.deb` and remove it
later without retaining the build tree:

```sh
sudo apt install /path/to/emacs-pgtk-local_31.1.1-1_amd64.deb
sudo apt remove emacs-pgtk-local
```

The PGTK port does not provide `xwidgets`, so its configure options omit
`--with-xwidgets`.
