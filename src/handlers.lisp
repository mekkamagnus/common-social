(in-package :common-social.handlers)

(defun render-post-html (post)
  "Render a single post as HTML"
  (let ((id (first post))
        (content (second post))
        (created-at (third post)))
    (cl-who:with-html-output-to-string (*standard-output*)
      (:div :class "bg-white rounded-lg shadow-sm border border-gray-200 p-4 mb-4"
       (:p :class "text-gray-900 mb-2" (cl-who:str content))
       (:div :class "text-sm text-gray-500"
        (cl-who:str created-at))))))

(defun render-home-page ()
  "Render the main timeline page"
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :lang "en"
     (:head 
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Common Social")
      (:script :src "https://cdn.tailwindcss.com")
      (:script :src "https://unpkg.com/htmx.org@1.9.10"))
     (:body :class "bg-gray-50 min-h-screen"
      ;; Header
      (:header :class "bg-white border-b border-gray-200"
       (:div :class "max-w-2xl mx-auto px-4 py-4"
        (:h1 :class "text-2xl font-bold text-gray-900" "🐦 Common Social")))
      
      ;; Main content
      (:main :class "max-w-2xl mx-auto px-4 py-6"
       ;; Post composer
       (:div :class "bg-white rounded-lg shadow-sm border border-gray-200 p-4 mb-6"
        (:form :hx-post "/posts" :hx-target "#timeline" :hx-swap "afterbegin"
         (:textarea :name "content" :rows "3" :maxlength "280"
                    :placeholder "What's happening?"
                    :class "w-full resize-none border-0 focus:ring-0 focus:outline-none text-lg"
                    :oninput "updateCharCount(this)")
         (:div :class "flex justify-between items-center mt-3 pt-3 border-t border-gray-100"
          (:span :id "char-count" :class "text-sm text-gray-500" "280")
          (:button :type "submit" 
                   :class "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded-full font-semibold disabled:opacity-50"
                   "Post"))))
       
       ;; Timeline
       (:div :id "timeline"
        (dolist (post (get-all-posts))
          (cl-who:str (render-post-html post)))))
      
      ;; Character counter script
      (:script 
       "function updateCharCount(textarea) {
          const remaining = 280 - textarea.value.length;
          const counter = document.getElementById('char-count');
          counter.textContent = remaining;
          counter.className = remaining < 20 ? 'text-sm text-red-500' : 'text-sm text-gray-500';
        }")))))

;; Request handlers
(defun handle-home ()
  "Handle home page requests"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (render-home-page))

(defun handle-create-post ()
  "Handle post creation"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((content (hunchentoot:parameter "content")))
    (if (create-post content)
        ;; Return the new post HTML for HTMX
        (let ((new-posts (get-all-posts :limit 1)))
          (when new-posts
            (render-post-html (first new-posts))))
        ;; Return error
        (cl-who:with-html-output-to-string (*standard-output*)
          (:div :class "bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4"
           "Failed to create post. Please check your input.")))))

(defun setup-routes ()
  "Setup HTTP routes"
  (hunchentoot:define-easy-handler (home :uri "/") () (handle-home))
  (hunchentoot:define-easy-handler (create-post-handler :uri "/posts") () (handle-create-post))
  (format t "✅ Routes configured~%"))