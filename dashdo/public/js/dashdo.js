(function ( $ ) {
 
    $.fn.dashdo = function(options) {
      /*
      options:

       basic:
        - ajax: true / false (default: false),
        - uuidUrl (default: '/uuid')
        - uuidInterval (default: 1000)
        - periodicSubmitSelector (default: '.dashdo-periodic-submit')
        - dashdoTitleSelector (default: '#dashdo-title')

       ui (ajax-only!):
        - container (which to (re-)render)
        - switcherElements: $('.dashdo-link'),
        - switcherAttr: 'href',
        - switcherEvent: 'click',
      */

      var settings = $.extend({
        // These are the defaults.
        ajax: false,
        uuidUrl: "/uuid",
        uuidInterval: 1000,
        
        // multiple dashdos only:
        containerElement: null,
        switcherElements: null,
        switcherAttr: 'href',
        switcherEvent: 'click',

        periodicSubmitSelector: '.dashdo-periodic-submit',
        dashdoTitleSelector: '#dashdo-title',
      }, options)

      var resubmitNatively = function() {
        $("input", this).prop('readonly', true);
        $(this).submit()
      }.bind(this)
      
      $("input,select", this).each(function(i,e) {
        $(e).change(function() {  // todo: _.debounce(func, [wait=0], [options={}]) ?
          if (typeof(manual_submit) == "undefined" || !manual_submit) {
            properReSubmit()
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

      var resubmitWithAjax = function(onSuccessfulRender) {
        if(!!settings.containerElement) {
          requestHtmlFromServer(
            $(this).attr("action"),
            $(this).serialize(),
            function(data) {
              $(settings.containerElement).html(data)
              if(!!onSuccessfulRender) {
                onSuccessfulRender()
              }
            }.bind(this)
          )
        }
      }.bind(this)

      var properReSubmit = (settings.ajax) ?
            resubmitWithAjax :
            resubmitNatively;

      var uuid = null
      var uuidLoop = function() {
        $.get(settings.uuidUrl).done(function(data) {
          if(uuid && uuid != data) {
            properReSubmit()
          }
          uuid = data
        }).always(function() {
          setTimeout(uuidLoop, settings.uuidInterval)
        })
      }
      uuidLoop()

      var submitTimer = null
      var periodicSubmitLoop = function() {
        var currentPeriodicSubmitValue = parseInt($(this).find(settings.periodicSubmitSelector).val())
        if(!!currentPeriodicSubmitValue) {
          properReSubmit()
          submitTimer = setTimeout(periodicSubmitLoop.bind(this), currentPeriodicSubmitValue)
        }
      }.bind(this)
      periodicSubmitLoop()

      var switchDashdo = function(endpoint) {
        $(this).attr("action", endpoint)  // set action of the form to the endpoint
        resubmitWithAjax(function() {  // if switched & rendered successfully, renew periodic submit loop
          if(!!settings.dashdoTitleSelector) {
            var title = $(settings.switcherElements)
            .filter('[' + settings.switcherAttr + '="' + endpoint + '"]')
            .text()

            if(!!title) {
              $(settings.dashdoTitleSelector).text(title)
            }
          }

          clearInterval(submitTimer)
          periodicSubmitLoop()
        })
      }.bind(this)

      if(settings.switcherElements !== null) {  // TODO: multi-dashdo == ajax ? remove ?
        var firstEndpoint = 
          $(settings.switcherElements)
            .first()
            .attr(settings.switcherAttr)

        if(firstEndpoint) {
          switchDashdo(firstEndpoint)

          $(settings.switcherElements).on(settings.switcherEvent, function(e) {
            e.preventDefault()
            switchDashdo($(e.target).attr(settings.switcherAttr))
          })
        }
      }
      
      // TODO: sidebar?
      // TODO: title
      // TODO: restyle
      // TODO: remove STATIC PILICY! wai-middleware-static

      return this;
    }
 
}( jQuery ));