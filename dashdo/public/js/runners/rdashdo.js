(function($) {
  $(document).ready(function() {
    $('#dashdo-form').dashdo({
      ajax: true,
      containerElement: $('#dashdo-main'),
      endpoints: $('.dashdo-link').map(function() { return this.href }).get(),
    })
  })
})(jQuery)