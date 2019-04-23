# pdf-view-restore

Support for opening last known pdf position in pdf-view-mode provided by [pdf-tools](https://github.com/politza/pdf-tools).

To install, add the following:

```lisp
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))
```
Once setup, pdf documents will automatically open to their last known page.

By default, save information will be saved relative to the pdf document. If you
do not want this behavior, add the following:

```lisp
(setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
```

Do note pdf documents with the same name will conflict with this setting, but you get
the ability to move pdf documents around and still keeping the save information in sync.
