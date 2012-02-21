;;; services.el --- manage services using emacs

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-02-20
;; Last changed: 2012-02-21 11:13:00
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl nil t))

(defstruct (service (:type list))
  "Service structure"
  name
  command
  dir
  buffer-name)

(defvar services-alist nil
  "List of all defined services.")

(defun defservice (name command &optional dir)
  "Create a new service named NAME using run by COMMAND."
  (let* ((name (if (symbolp name) name (intern name)))
	 (service
	  (make-service :name name
			:command command
			:dir dir
			:buffer-name (format "*service:%s*"
					     (symbol-name name)))))
    (when (assoc name services-alist)
      (setq services-alist (assq-delete-all name services-alist)))
    (add-to-list 'services-alist  service)))

;;;###autoload
(defun service-start (name)
  "Run service NAME."
  (interactive
   (list (intern
	  (completing-read
	   "Run service: "
	   (mapcar 'symbol-name (services-available))
	   nil t))))
  (let* ((service (or (assoc name services-alist)
		      (error "Service %s not found."
			     (symbol-name name))))
	 (buffer (get-buffer-create (service-buffer-name service)))
	 (default-directory (or (service-dir service)
				default-directory))
	 (cmd (service-command service))
	 (proc (apply 'start-process (symbol-name name) buffer (car cmd) (cdr cmd))))
    (set-process-sentinel
     proc
     (lambda (proc change)
       (let ((status (process-status proc))
	     (exit-status (process-exit-status proc))
	     (buffer (process-buffer proc))
	     (name (process-name proc)))
       (cond
	;; killed by signal
	((eq status 'signal)
	 (message "Process %s killed by signal %d." name exit-status)
	 (kill-buffer buffer))
	;; exit normally
	((and (eq status 'exit) (eq 0 exit-status))
	 (message "Process %s finished normally." name)
	 (kill-buffer buffer))
	;; exit abnormally
	((and (eq status 'exit) (not (eq 0 exit-status)))
	 (message "Process %s finished abnormally (%d)." name exit-status)
	 (switch-to-buffer buffer))))))))

;;;###autoload
(defun service-stop (name)
  "Stop service NAME."
  (interactive
   (list (intern
	  (completing-read
	   "Stop service: "
	   (mapcar 'symbol-name (services-running))
	   nil t))))
  (with-service
   name
   (kill-process (get-buffer-process (current-buffer)))))


(defmacro with-service (name body)
  "Run BODY for service defined by NAME.

Define variables service and buffer
"
  `(let* ((name ,name)
	  (service (or (assoc name services-alist)
		       (error "Service %s not found."
			      (symbol-name name))))
	  (buffer (or (get-buffer (service-buffer-name service))
		      (error "Buffer running %s not found (%s)."
			     (symbol-name name)
			     (service-buffer-name service)))))
     (with-current-buffer buffer
       ,body)))

(defun services-available ()
  "Return a list of all available services."
  (interactive)
  (loop for (k . v) in services-alist
	collect k))

(defun services-running ()
  "Return a list of all running services."
  (interactive)
  (loop for (k . v) in services-alist
	when (service-is-running-p k)
	collect k))


(defun service-is-running-p (name)
  "Test if service NAME is running or not."
  (ignore-errors
    (with-service
     name
     (if (get-buffer-process (current-buffer)) t nil))))

(provide 'services)
