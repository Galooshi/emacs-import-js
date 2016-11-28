[![MELPA](http://melpa.org/packages/import-js-badge.svg)](http://melpa.org/#/import-js)

# Running ImportJS in Emacs

1. Install the `importjs` binary:
  * `npm install import-js -g`
2. Configure ImportJS
  * See [Configuration](README.md#configuration)
3. Install import-js.el for Emacs
  * Install via [MELPA](https://melpa.org/#/import-js)
  * Alternatively, Copy plugins/import-js.el into your Emacs load-path and add
    `(require 'import-js)` to your config. You will also need to install
    [grizzl](https://github.com/grizzl/grizzl)
4. Configure your project root
  * `(setq import-js-project-root "/path/to/project")`
5. Import a file!
  * You can use something like `(M-x) import-js-import` with your cursor over
    the desired module
  * It will be helpful to bind `import-js-import` to an easy-to-use binding,
    such as:

    ```
    (define-prefix-command 'my-keymap)
    (global-set-key (kbd "s-a") 'my-keymap)
    (define-key my-keymap (kbd "a u") 'import-js-import)
    ```
6. Go directly to a file
  * The ImportJS goto interface allows us to jump to a package
  * `(M-x) import-js-goto` will jump to the appropriate file found by ImportJS
  * This should also be bound to something useful:
    `(global-set-key (kbd "<f4>") 'import-js-goto)`
7. Fix your imports
  * Optionally, you can configure ImportJS to fix your imports for you, adding
    unknown variables and removing unused imports. ImportJS uses eslint to find
    these variables.
  * `eslint` must be in your PATH.
  * eslint plugins must be installed for that specific version of eslint (if
    eslint is a global eslint, you may need to install the plugins globally)
  * Run with `(M-x) import-js-fix`
  * You can also configure `import-js-fix` to run on save:
    `(add-hook 'after-save-hook 'import-js-fix)`

# Note on Node v6.1.0

I've had issues running import-js in Emacs on Node v6.1.0. Node >= v6.2.0 seems
to work as expected.
