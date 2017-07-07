(function ( $ ) {
 
    $.fn.dashdo = function(options) {
      /*
      options:

       basic:
        - ajax: true / false (default: false),
        - uuidUrl (default: '/uuid')
        - uuidInterval (default: 1000)
        - periodicSubmitSelector (default: '.dashdo-periodic-submit')

       ui (ajax-only!):
        - dashdoTitleSelector (default: '#dashdo-title')
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

      $(this).on("change", ":input", function() {
        if (typeof(manual_submit) === "undefined" || !manual_submit) {
          $(":input").prop("readonly", true);
          properReSubmit()
        }
      })

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
              
              $('.dashdo-plotly-select .js-plotly-plot').each(function() {
                var graph = $(this).get(0);
                var attr = $(this).siblings('.dashdo-plotly-select-attr').attr('value');
                var input = $(this).siblings('input').first();

                var _restyle = function() {
                  var value = input.attr('value');
                  if (value === "") {
                    Plotly.restyle(graph, { 'marker.color': '#1F77B4)' });
                  } else {
                    var os = graph.data[0][attr].map(function(p) {  // example: graph.data[0]['y']
                      return p == value ? '#1F77B4' : '#A5C8E1';
                    });
                    Plotly.restyle(graph, {'marker.color' : [os]}, [0]);
                  }
                };
                _restyle();

                $(this).get(0).on('plotly_click', function(data) {
                  if (input.attr('value') == data.points[0][attr]) {
                    input.attr('value', "");
                  } else {
                    input.attr('value', data.points[0][attr]);
                  }
                  _restyle()
                  input.change()  // to fix: input.change causes resubmit that 'looses' .dashdo-plotly-select events
                });
              });

              // if switched & rendered successfully, renew periodic submit loop
              clearInterval(submitTimer)
              periodicSubmitLoop()
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

      // cosmetics
      var refreshTitle = function(endpoint) {
        if(!!settings.dashdoTitleSelector) {
          var title = $(settings.switcherElements)
          .filter('[' + settings.switcherAttr + '="' + endpoint + '"]')
          .text()

          if(!!title) {
            $(settings.dashdoTitleSelector).text(title)
          }
        }
      }

      var switchDashdo = function(endpoint) {
        $(this).attr("action", endpoint)  // set action of the form to the endpoint
        refreshTitle(endpoint)
        resubmitWithAjax()
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

      return this; // returning the dashdo
    }
 
}( jQuery ));