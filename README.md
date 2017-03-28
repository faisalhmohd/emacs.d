# My Emacs Config

## Installing

1. Close Emacs.
2. Delete `~/.emacs` or `~/.emacs.d` if they exist. (Windows users, your
   emacs files will probably live in
   `C:\Users\your_user_name\AppData\Roaming\`. So, for example, you
   would delete `C:\Users\faisal\AppData\Roaming\.emacs.d`.) This is
   where Emacs looks for configuration files, and deleting these files
   and directories will ensure that you start with a clean slate.
3. Download the Emacs
   [configuration zip file](https://github.com/faisalhmohd/emacs.d/archive/book1.zip)
   and unzip it. Its contents should be a folder,
   `emacs.d`. Run `mv ./emacs.d ~/.emacs.d`.
4. [Optional - If you need clojure config] Create the file `~/.lein/profiles.clj` (Windows users, this is
   probably `C:\Users\your_user_name\.lein\profiles.clj`) and add this
   line to it:

```clojure
{:user {:plugins [[cider/cider-nrepl "0.8.1"]]}} 
```

Then open Emacs.

## Upgrading

Before upgrading, ensure that your `.emacs.d` directory is under
version control so that you can always revert to a known good state.

To upgrade:

1. Edit `.emacs.d/init.el`, adding these lines after line 12:

   ```elisp
   (add-to-list 'package-archives
                '("melpa-stable" . "http://stable.melpa.org/packages/") t)
   
   (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
   ```

2. Close Emacs.
3. Run `rm -Rf .emacs.d/elpa/cider-*`
4. Open Emacs. You'll probably see some errors and your theme won't
   load. That's ok.
5. In Emacs, run `M-x package-refresh contents`.
6. In Emacs, run `M-x package-install cider`.
7. Close and re-open Emacs.
8. Open `.lein/profiles.clj` and remove `[cider/cider-nrepl "0.8.1"]` from it.

That should install the latest version. Enjoy!

## Organization

I've tried to separate everything logically and document the purpose
of every line. [`init.el`](./init.el) acts as a kind of table of
contents.  It's a good idea to eventually go through `init.el` and the
files under the `customizations` directory so that you know exactly
what's going on.
