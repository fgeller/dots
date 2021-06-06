(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		mode-line-position
		evil-mode-line-tag
		(vc-mode vc-mode)
		"  "
		;;mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))
