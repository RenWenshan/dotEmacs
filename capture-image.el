;; screenshot in org-mode

;;get plugin file path
(setq now-path load-file-name)
(defun iyf-screenshot ()
    (interactive)
    (if (equal buffer-file-name nil) (message "Not in a buffer!") 
    (progn
      (setq jarpath (concat (file-name-directory now-path) "CaptureImage.jar"))
      (setq filename (concat (make-temp-name  "images/" ) ".jpg"))
      (if (file-accessible-directory-p (concat (file-name-directory (buffer-file-name)) "images/")) nil (make-directory "images"))
      (call-process-shell-command (concat "java -jar " jarpath " \"" filename "\"" ))
      (insert (concat "[[./" filename "]]"))
      (org-display-inline-images)
    ))
 ) 
(provide 'capture-image)
