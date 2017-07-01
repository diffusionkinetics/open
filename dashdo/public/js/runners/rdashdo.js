(function($) {
  $(document).ready(function() {
    $('#dashdo-form').dashdo({
      ajax: true,
      containerElement: $('#dashdo-main'),
      switcherElements: $('.dashdo-link'),
    })
  })
})(jQuery)