$(function(){
  var resubmit = function() {
    $("input").prop('readonly', true);
    $('#dashdoform').submit()
  }

  $('#dashdoform input,select').each(function(i,e) {
    $(e).change(function() {
      if (!(manual_submit||false))
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

});
