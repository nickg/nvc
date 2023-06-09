## Hacking

NVC always serves the static HTML and JS files from the install prefix.
Development using `make`, `make install`, and then reloading is way too
slow so use the [https://vitejs.dev/](Vite) development server instead
by running `nvc --gui` and then `npm start` in this directory in another
window.  The open [http://localhost:5173] in your web browser (note the
different port number).  This will automatically reload when any of the
source files changes.

