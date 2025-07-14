(in-package :common-social.handlers)

(defun render-base-template (title content)
  "Render base HTML template using CL-WHO"
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
     (:head 
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title (cl-who:str title))
      
      ;; Tailwind CSS
      (:script :src "https://cdn.tailwindcss.com")
      
      ;; HTMX
      (:script :src "https://unpkg.com/htmx.org@1.9.10")
      
      ;; Custom styles
      (:style :type "text/css"
              ".touch-action-manipulation { touch-action: manipulation; }
               .char-counter-ok { color: #6b7280; }
               .char-counter-warning { color: #f59e0b; }
               .char-counter-danger { color: #ef4444; }
               .htmx-request { opacity: 0.5; }
               .htmx-request .loading-spinner { display: inline-block; }
               .loading-spinner { display: none; }"))
     (:body :class "bg-gray-50 min-h-screen"
      ;; Header
      (:header :class "bg-white border-b border-gray-200 sticky top-0 z-50"
       (:div :class "max-w-2xl mx-auto px-4 py-3"
        (:div :class "flex items-center justify-between"
         (:h1 :class "text-xl font-bold text-gray-900" "🐦 Common Social")
         (:div :class "text-sm text-gray-500" "MVP"))))
      
      ;; Main content
      (:main :class "max-w-2xl mx-auto"
       (cl-who:str content))
      
      ;; HTMX configuration script
      (:script :type "text/javascript"
               "htmx.config.globalViewTransitions = true;
                htmx.config.requestClass = 'htmx-request';
                
                function updateCharCounter(textarea) {
                    const maxLength = 280;
                    const currentLength = textarea.value.length;
                    const remaining = maxLength - currentLength;
                    const counter = document.getElementById('char-count');
                    const submitBtn = document.getElementById('submit-btn');
                    
                    if (counter) {
                        counter.textContent = remaining;
                        counter.className = '';
                        if (remaining < 20) {
                            counter.classList.add('char-counter-danger');
                        } else if (remaining < 50) {
                            counter.classList.add('char-counter-warning');
                        } else {
                            counter.classList.add('char-counter-ok');
                        }
                    }
                    
                    if (submitBtn) {
                        const trimmedLength = textarea.value.trim().length;
                        submitBtn.disabled = trimmedLength === 0 || currentLength > maxLength;
                        
                        if (submitBtn.disabled) {
                            submitBtn.classList.add('opacity-50', 'cursor-not-allowed');
                            submitBtn.classList.remove('hover:bg-blue-600');
                        } else {
                            submitBtn.classList.remove('opacity-50', 'cursor-not-allowed');
                            submitBtn.classList.add('hover:bg-blue-600');
                        }
                    }
                }
                
                htmx.on('htmx:afterSwap', function(evt) {
                    if (evt.detail.target.id === 'timeline') {
                        const form = document.getElementById('post-form');
                        const textarea = document.getElementById('post-content');
                        if (form && textarea) {
                            form.reset();
                            updateCharCounter(textarea);
                            textarea.focus();
                        }
                    }
                });")))))

(defun render-timeline-posts (posts)
  "Render timeline posts using CL-WHO"
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (dolist (post posts)
     (:article :class "bg-white p-4 hover:bg-gray-50 transition-colors duration-150"
      (:div :class "flex space-x-3"
       ;; Avatar placeholder
       (:div :class "flex-shrink-0"
        (:div :class "w-10 h-10 bg-gradient-to-br from-blue-400 to-blue-600 rounded-full flex items-center justify-center"
         (:span :class "text-white font-medium" "👤")))
       
       ;; Post content
       (:div :class "flex-1 min-w-0"
        ;; Header
        (:div :class "flex items-center space-x-2 mb-1"
         (:span :class "font-medium text-gray-900" "You")
         (:span :class "text-gray-500" "·")
         (:time :class "text-gray-500 text-sm" 
                :title (cdr (assoc "created_at" post :test #'string=))
                (cl-who:str (cdr (assoc "relative_time" post :test #'string=)))))
        
        ;; Post text
        (:div :class "text-gray-900 leading-relaxed whitespace-pre-wrap break-words"
         (cl-who:str (cdr (assoc "content" post :test #'string=))))))))
    
    ;; Load more posts (for future pagination)
    (when (>= (length posts) 20)
     (:div :class "bg-white p-4 text-center"
      (:button :class "text-blue-500 hover:text-blue-600 font-medium py-2 px-4 min-h-[44px] touch-action-manipulation focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
               "Load more posts")))))

(defun render-timeline-page (posts post-count)
  "Render complete timeline page using CL-WHO"
  (let ((timeline-content 
         (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
           ;; Post composer
           (:div :class "bg-white border-b border-gray-200 p-4"
            (:form :id "post-form" :hx-post "/posts" :hx-target "#timeline" :hx-swap "innerHTML" :class "space-y-3"
             
             ;; Textarea
             (:div 
              (:textarea :id "post-content" :name "content" :placeholder "What's happening?"
                         :class "w-full p-3 border border-gray-300 rounded-lg resize-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 text-base leading-relaxed touch-action-manipulation"
                         :rows "3" :maxlength "280" :oninput "updateCharCounter(this)" :autofocus t))
             
             ;; Controls
             (:div :class "flex items-center justify-between"
              (:div :class "flex items-center space-x-2"
               (:span :class "text-sm"
                (:span :id "char-count" :class "char-counter-ok" "280")
                (:span :class "text-gray-400" " characters remaining")))
              
              (:button :id "submit-btn" :type "submit" :disabled t
                       :class "bg-blue-500 text-white px-6 py-2 rounded-full font-medium min-h-[44px] min-w-[80px] touch-action-manipulation opacity-50 cursor-not-allowed focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
                       (:span :class "loading-spinner" "⏳")
                       "Post"))))
           
           ;; Timeline
           (:div :id "timeline" :class "divide-y divide-gray-200"
            (cl-who:str (render-timeline-posts posts)))
           
           ;; Empty state
           (when (null posts)
            (:div :class "text-center py-12 px-4"
             (:div :class "text-6xl mb-4" "📝")
             (:h3 :class "text-lg font-medium text-gray-900 mb-2" "No posts yet")
             (:p :class "text-gray-500 mb-6" "Share your first thought with the world!")))
           
           ;; Stats footer
           (:div :class "bg-white border-t border-gray-200 p-4 text-center text-sm text-gray-500"
            (cl-who:fmt "Total posts: ~A" (or post-count 0))))))
    (render-base-template "Home - Common Social" timeline-content)))

(defun json-response (data)
  "Return JSON response with proper headers"
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string data))

(defun handle-home ()
  "Handle home timeline page"
  (let ((posts (common-social.models:get-all-posts :limit 20)))
    (render-timeline-page (mapcar #'format-post-for-template posts)
                          (common-social.models:get-post-count))))

(defun handle-create-post ()
  "Handle post creation"
  (let ((content (hunchentoot:parameter "content")))
    (handler-case
        (progn
          (common-social.models:create-post content)
          ;; Return updated timeline fragment for HTMX
          (let ((posts (common-social.models:get-all-posts :limit 20)))
            (render-timeline-posts (mapcar #'format-post-for-template posts))))
      (error (e)
        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
        (json-response `(("error" . ,(format nil "~A" e))))))))

(defun handle-timeline-partial ()
  "Handle timeline partial for HTMX updates"
  (let ((posts (common-social.models:get-all-posts :limit 20)))
    (render-timeline-posts (mapcar #'format-post-for-template posts))))

(defun format-post-for-template (post-row)
  "Format post data for template rendering"
  (let ((id (first post-row))
        (content (second post-row))
        (created-at-string (third post-row)))
    `(("id" . ,id)
      ("content" . ,(common-social.utils:escape-html content))
      ("created_at" . ,created-at-string)
      ("relative_time" . ,(common-social.utils:timestamp-to-relative-string 
                          (local-time:parse-timestring created-at-string))))))

(defun setup-routes ()
  "Define application routes"
  
  ;; Home timeline
  (hunchentoot:define-easy-handler (home :uri "/") ()
    (handle-home))
  
  ;; Create post (HTMX endpoint)
  (hunchentoot:define-easy-handler (create-post :uri "/posts") ()
    (when (eq (hunchentoot:request-method*) :post)
      (handle-create-post)))
  
  ;; Timeline partial for HTMX
  (hunchentoot:define-easy-handler (timeline-partial :uri "/timeline") ()
    (handle-timeline-partial))
  
  ;; Hot loading status API
  (hunchentoot:define-easy-handler (hotload-status :uri "/hotload-status") ()
    (setf (hunchentoot:content-type*) "application/json")
    (cl-json:encode-json-to-string (common-social.hotload:hotload-status)))
  
  ;; Static files
  (hunchentoot:define-easy-handler (static-files :uri "/static/*") ()
    (let ((path (merge-pathnames 
                 (subseq (hunchentoot:script-name*) 8)
                 common-social.config:*static-directory*)))
      (when (probe-file path)
        (hunchentoot:handle-static-file path)))))