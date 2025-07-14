// Additional JavaScript functionality for Common Social MVP

// Enhanced mobile optimizations
document.addEventListener('DOMContentLoaded', function() {
    // Prevent zoom on input focus (iOS optimization)
    const viewport = document.querySelector('meta[name=viewport]');
    if (viewport) {
        viewport.setAttribute('content', 
            'width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0');
    }
    
    // Add touch feedback for buttons
    const buttons = document.querySelectorAll('button');
    buttons.forEach(button => {
        button.addEventListener('touchstart', function() {
            this.style.transform = 'scale(0.98)';
        });
        
        button.addEventListener('touchend', function() {
            this.style.transform = 'scale(1)';
        });
    });
    
    // Auto-resize textarea based on content
    const textarea = document.getElementById('post-content');
    if (textarea) {
        textarea.addEventListener('input', function() {
            this.style.height = 'auto';
            this.style.height = Math.min(this.scrollHeight, 200) + 'px';
        });
    }
    
    // Scroll to top on new post
    htmx.on('htmx:afterSwap', function(evt) {
        if (evt.detail.target.id === 'timeline') {
            window.scrollTo({ top: 0, behavior: 'smooth' });
        }
    });
});

// Service worker registration for future PWA features
if ('serviceWorker' in navigator) {
    // Will be implemented in future phases
}