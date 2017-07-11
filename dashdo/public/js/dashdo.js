(function ( $ ) {
 
    $.fn.dashdo = function(options) {
      /*
      options:

       basic:
        - ajax: true / false (default: false),
        - uuidUrl (default: '/uuid')
        - uuidInterval (default: 1000)
        - periodicSubmitSelector (default: '.dashdo-periodic-submit')
        - colorSelected: 1F77B4
        - colorUnSelected: A5C8E1

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
        uuidUrl: '/uuid',
        uuidInterval: 1000,

        colorSelected: '#1F77B4',
        colorUnSelected: '#A5C8E1',
        
        containerElement: null,
        switcherElements: null,
        switcherAttr: 'href',
        switcherEvent: 'click',

        periodicSubmitSelector: '.dashdo-periodic-submit',
        dashdoTitleSelector: '#dashdo-title',

        multiselectNamesSelector: '.dashdo-plotly-multi-select-names',
      }, options)

      var resubmitNatively = function() {
        $('input', this).prop('readonly', true)
        $(this).submit()
      }.bind(this)

      $(this).on('change', ':input', function() {
        if (typeof(manual_submit) === 'undefined' || !manual_submit) {
          $(':input').prop('readonly', true)
          properReSubmit()
        }
      })

      this.filter('form').on('submit', function(e) {
        if(!settings.ajax) {
          $('input', this).prop('readonly', true)
          return // and then it actualy submits
        }
        e.preventDefault()  // no 'native' submitting on ajax versions
      })

      var requestHtmlFromServer = function(url, data, onSuccess) {
        $.ajax({
          type: 'POST',
          url: url,
          data: data,
          success: onSuccess,
        })
      }

      var resubmitWithAjax = function(onSuccessfulRender) {
        if(!!settings.containerElement) {
          requestHtmlFromServer(
            $(this).attr('action'),
            $(this).serialize(),
            function(data) {
              $(settings.containerElement).html(data)
              
              $('.dashdo-plotly-select .js-plotly-plot').each(function() {
                var graph = $(this).get(0)
                var axis = (graph.data[0].orientation === 'h') ? 'y' : 'x'

                var values = $(this).siblings('input[name]').map(function() {  // name must be specified!
                  return this.value
                }).get()
                
                var restyle = function() {
                  if (values.length === 0) {
                    // making all graph selected
                    console.log('all are selected! ^))')
                    Plotly.restyle(graph, { 'marker.color': settings.colorSelected })
                  } else {
                    var os = graph.data[0][axis].map(function(p) {  // example: graph.data[0]['y']
                      return (values.indexOf(p) !== -1) ? // p `elem` values == True
                        settings.colorSelected : 
                        settings.colorUnSelected
                    })
                    Plotly.restyle(graph, {'marker.color' : [os]}, [0])
                  }
                }
                restyle()

                var currentMultipleFieldName = $(this).siblings(settings.multiselectNamesSelector).val()
                var isMultiple = !!currentMultipleFieldName

                $(this).get(0).on('plotly_click', function(data) {
                  var selectedValueFromPlot = data.points[0][axis]

                  if(!isMultiple) {
                    var input = $(this).siblings('input[name]').first()
                    if (input.attr('value') == selectedValueFromPlot) { // if selected
                      input.attr('value', '') // then deselect
                    } else {
                      input.attr('value', selectedValueFromPlot)  // if not selected, then select
                    }
                  } else {
                    var currentInputs = $(this).siblings('input[name="' + currentMultipleFieldName + '"][value="' + selectedValueFromPlot + '"]')
                    
                    if (currentInputs.length > 0) {
                      $(currentInputs).remove()
                    } else {
                      $(this).after('<input type="hidden" name="' + currentMultipleFieldName + '" value="' + selectedValueFromPlot + '">')
                    }
                  }

                  properReSubmit()
                }.bind(this))
              })

              // if switched & rendered successfully, renew periodic submit loop
              clearInterval(submitTimer)
              periodicSubmitLoop()
            }.bind(this)
          )
        }
      }.bind(this)

      var properReSubmit = (settings.ajax) ?
            resubmitWithAjax :
            resubmitNatively

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
        $(this).attr('action', endpoint)  // set action of the form to the endpoint
        refreshTitle(endpoint)
        resubmitWithAjax()
      }.bind(this)

      if(settings.switcherElements !== null) {
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

      return this // returning the dashdo
    }
 
}( jQuery ))