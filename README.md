
<div class="header" align="center">

# mr-emacs

<div class="logo">
<p align="center">
<img src="./media/mr-emacs-mascott.png" alt="mr-emacs-mascott.png" width="100px" />
<br>
A minimal emacs configuration.
</p>
</div>

</div>



## Motivation

Emacs is great out of the box. This project aims to create a simple configuration which contains the minimum viable components to make it a complete user interface.

## Contents

``` sh
    mr-emacs/
      modules/    # modular configuration files (i.e. python, motion, etc)
      mr-simple.el   # the core file containing the initialization filer
	  init.el     # symlink this file to ~/.emacs.d/init.el
```

-   [x] [base configuration](./mr-simple.el) basic configuration settings (i.e. fonts, colorscheme, package setup, etc.)
-   [x] [motions](modules/mr-motion.el) : use avy for editing and other movement settings
-   [x] [orgmode](modules/mr-orgmode.el)
-   [x] [python](modules/mr-python.el)
-   [-] [eshell](modules/mr-eshell.el)
-   [-] [ai](modules/mr-ai.el) : (whisper and GPT calls)
-   [ ] [modeline](modules/mr-modeline.el)
