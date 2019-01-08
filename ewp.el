(require 'auth-source)
(require 'cl)
(require 'request)

(defcustom ewp-wordpress-url nil
  "Your Wordpress blog URL.")

(defcustom ewp-blog-dir "~/blog"
  "Local directory for the blog")

(defun ewp-get-auth ()
  "Returns user creds"
  (let ((credentials (auth-source-user-and-password ewp-wordpress-url)))
    (concat "Basic " (base64-encode-string (concat (car credentials) ":" (cadr credentials))))))

(defun ewp-get-posts ()
  "Get the blog posts and create files."
  (let ((post-api-url (concat ewp-wordpress-url "/wp-json/wp/v2/posts")))
    (request
     post-api-url
     :type "GET"
     :parser 'json-read
     :headers `(("Authorization" . ,(ewp-get-auth)))
     :success
     (cl-function (lambda (&key response &allow-other-keys)
                    (when response
                      (mapc
                       (lambda (e)
                         (ewp-process-and-create-file e))
                       (request-response-data response))))))))

(defun ewp-process-and-create-file (post)
  (let* ((post-content (cdr (car (alist-get 'content post))))
         (post-date (alist-get 'date post))
         (post-id (number-to-string (alist-get 'id post)))
         (post-title (cdr (car (alist-get 'title post))))
         (post-slug (alist-get 'slug post))
         (post-url (cdr (car (alist-get 'guid post))))
         (filename (concat post-id "_" post-slug ".org"))
         (temp-html-file (concat "/tmp/_" post-id ".html")))
    (with-current-buffer (find-file-noselect temp-html-file)
      (erase-buffer)
      (insert post-content)
      (save-buffer)
      (kill-buffer))
    (with-current-buffer (find-file-noselect (concat ewp-blog-dir "/" filename))
      (erase-buffer)
      (insert (concat "#+TITLE: " post-title "\n"))
      (insert (concat "#+ID: " post-id "\n"))
      (insert (concat "#+URL: " post-url "\n\n"))
      (shell-command (concat "pandoc -f html -t org " temp-html-file) t)
      (save-buffer))
    (delete-file temp-html-file)))

(defun ewp-create-post (title content status)
  "Create a new post to the blog."
  (let ((post-api-url (concat ewp-wordpress-url "/wp-json/wp/v2/posts"))
        (user-creds (auth-source-user-and-password ewp-wordpress-url)))
    (request
     post-api-url
      :type "POST"
      :data `(("title" . ,title)
              ("content" . ,content)
              ("status" . ,status))
      :parser 'json-read
      :headers `(("Authorization" . ,(concat "Basic "
                                             (base64-encode-string
                                              (concat
                                               (car user-creds)
                                               ":"
                                               (cadr user-creds))))))
      :success
      (cl-function (lambda (&key data &allow-other-keys)
                     (message "POSTED!"))))))

(defun ewp-sync ()
  "Syncs `ewp-blog-dir' with Wordpress (set by `ewp-wordpress-url').

If new files are found in `ewp-blog-dir', creates posts for each new file.
After finishing creating the posts, calls back Wordpress to get the updated
information and synchronizes all the local files."
  (interactive)
  (ewp-get-posts))
