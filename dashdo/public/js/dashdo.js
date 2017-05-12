$(function(){
  var resubmit = function() {
    $("input").prop('readonly', true);
    $('#dashdoform').submit()
  }

  $('#dashdoform input,select').each(function(i,e) {
    $(e).change(function() {
      if (typeof(manual_submit) == "undefined" || !manual_submit)
        resubmit();
    });
  });

  var uuid = null;

  var uuidLoop = function() {
    $.get("/uuid").done(function(data) {
      if(uuid && uuid != data)
        resubmit();
      uuid = data;

    }).always(function() {
      setTimeout(uuidLoop, 1000);
    });
  };

  uuidLoop();

  // hide all dashdos except the first
  $('.dashdo').slice(1).hide();

  // add submit handlers
  $('.dashdo').submit(function(e) {
    var dashdo = $(this);
    $.ajax({
      type: "POST",
      url: $(this).attr('id'),
      data: $(this).serialize(),
      success: function(r) {
        dashdo.html(r);
      }
    });
    e.preventDefault();
  });
});
