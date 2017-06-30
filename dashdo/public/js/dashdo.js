// TODO: in RDashdo form 'href' -> 'action' (to make it submit!)
// TODO: ajax rendering
// TODO: switch between dashdos
// TODO: remove STATIC PILICY! wai-middleware-static
(function ( $ ) {
 
    $.fn.dashdo = function(options) {
      /*
      options:

       basic:
        - ajax: true / false (default: false),
        - uuidUrl (default: '/uuid')
        - uuidInterval (default: 1000)

       ui:
        - event on which dashdo changes
        - (ajax-only!) container (which to (re-)render)
      */

      var settings = $.extend({
        // These are the defaults.
        ajax: false,
        uuidUrl: "/uuid",
        uuidInterval: 1000,
      }, options )

      var resubmit = function() {
        $("input", this).prop('readonly', true);
        $(this).submit()
      }.bind(this)
      
      $("input,select", this).each(function(i,e) {
        $(e).change(function() {  // todo: _.debounce(func, [wait=0], [options={}]) ?
          if (typeof(manual_submit) == "undefined" || !manual_submit) {
            resubmit()
          }
        });
      }.bind(this));

      this.filter("form").on("submit", function(e) {
        if(!settings.ajax) {
          $("input", this).prop('readonly', true);
          return // and then it actualy submits
        }
        e.preventDefault()  // no 'native' submitting on ajax versions
      })

      var requestHtmlFromServer = function(url, data, onSuccess) {
        $.ajax({
          type: "POST",
          url: url,
          data: data,
          success: onSuccess,
        })
      }

      var submitWithAjax = function() {
        requestHtmlFromServer(
          $(e.target).attr("action"),
          $(e.target).serialize(),
          function(data) {
            // TODO: ajax rendering
            console.log('data', data)
            console.log('this', this)
          }.bind(this)
        )
      }.bind(this)

      var uuid = null
      var uuidLoop = function() {
        $.get(settings.uuidUrl).done(function(data) {
          if(uuid && uuid != data) {
            resubmit()
          }
          uuid = data
        }).always(function() {
          setTimeout(uuidLoop, settings.uuidInterval)
        })
      }
      uuidLoop()

      // TODO: #2 - uuid chage
      // TODO: #3 - periodic submit
      // TODO: #4 - manual submit

      return this;
    }
 
}( jQuery ));