# mr-emacs



<div class="logo">
<p align="center">
<img src="./media/mr-emacs-mascott.png" alt="mr-emacs-mascott.png" width="100px" />
A minimal emacs configuration.
</p>
</div>


## Motivation

Emacs is great out of the box. This project aims to create a simple configuration which contains the minimum viable components to make it a complete user interface.

## Contents

``` sh
    mr-emacs/
      modules/    # modular configuration files (i.e. python, motion, etc)
      today/      # today.el is a major-mode to track task
      themes/     # a light and dark themer

      simple.el   # the core file containing the initialization filer
      init.el     # symlink this file to ~/.emacs.d/init.el
```

-   [ ] [base configuration](./mr-simple.el) basic configuration settings (i.e. fonts, colorscheme, package setup, etc.)
-   [ ] [motions](modules/mr-motion.el) : god-mode modal editing and other movement settings
-   [ ] [orgmode](modules/mr-orgmode.el)
-   [ ] [python](modules/mr-python.el)
-   [ ] [eshell](modules/mr-eshell.el)
-   [-] [today](today/today.el) : a simple task tracker (WIP)
-   [ ] [ai](modules/mr-ai.el) : (whisper and GPT calls)
