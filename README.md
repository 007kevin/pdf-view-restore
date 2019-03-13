# pdf-view-restore

Support for opening last known pdf position in pdf-view-mode provided by [pdf-tools](https://github.com/politza/pdf-tools).

To install, add the following:

```lsip
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (pdf-view-restore-setup))
```
Once setup, pdf documents will automatically open to their last known page.
