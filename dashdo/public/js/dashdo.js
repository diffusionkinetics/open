(function ( $ ) {
  var shadeBlendConvert = function(p, from, to) {
    // taken from https://stackoverflow.com/questions/5560248/programmatically-lighten-or-darken-a-hex-color-or-rgb-and-blend-colors
    if(typeof(p)!="number"||p<-1||p>1||typeof(from)!="string"||(from[0]!='r'&&from[0]!='#')||(typeof(to)!="string"&&typeof(to)!="undefined"))return null; //ErrorCheck
    if(!this.sbcRip)this.sbcRip=function(d){
        var l=d.length,RGB=new Object();
        if(l>9){
            d=d.split(",");
            if(d.length<3||d.length>4)return null;//ErrorCheck
            RGB[0]=i(d[0].slice(4)),RGB[1]=i(d[1]),RGB[2]=i(d[2]),RGB[3]=d[3]?parseFloat(d[3]):-1;
        }else{
            if(l==8||l==6||l<4)return null; //ErrorCheck
            if(l<6)d="#"+d[1]+d[1]+d[2]+d[2]+d[3]+d[3]+(l>4?d[4]+""+d[4]:""); //3 digit
            d=i(d.slice(1),16),RGB[0]=d>>16&255,RGB[1]=d>>8&255,RGB[2]=d&255,RGB[3]=l==9||l==5?r(((d>>24&255)/255)*10000)/10000:-1;
        }
        return RGB;}
    var i=parseInt,r=Math.round,h=from.length>9,h=typeof(to)=="string"?to.length>9?true:to=="c"?!h:false:h,b=p<0,p=b?p*-1:p,to=to&&to!="c"?to:b?"#000000":"#FFFFFF",f=sbcRip(from),t=sbcRip(to);
    if(!f||!t)return null; //ErrorCheck
    if(h)return "rgb("+r((t[0]-f[0])*p+f[0])+","+r((t[1]-f[1])*p+f[1])+","+r((t[2]-f[2])*p+f[2])+(f[3]<0&&t[3]<0?")":","+(f[3]>-1&&t[3]>-1?r(((t[3]-f[3])*p+f[3])*10000)/10000:t[3]<0?f[3]:t[3])+")");
    else return "#"+(0x100000000+(f[3]>-1&&t[3]>-1?r(((t[3]-f[3])*p+f[3])*255):t[3]>-1?r(t[3]*255):f[3]>-1?r(f[3]*255):255)*0x1000000+r((t[0]-f[0])*p+f[0])*0x10000+r((t[1]-f[1])*p+f[1])*0x100+r((t[2]-f[2])*p+f[2])).toString(16).slice(f[3]>-1||t[3]>-1?1:3);
  }

  var sortSerializedString = function(s, fieldName) {
    if (typeof(fieldName) === 'undefined' || !fieldName) {
      return s
    }

    var pairStr = s.split("&")
    var fieldNameWithEqualSign = fieldName + "="
    pairStr.sort(function(a, b) {
      if (a.startsWith(fieldNameWithEqualSign) && !b.startsWith(fieldNameWithEqualSign)) {
        return true // a goes to the end
      } else if (!a.startsWith(fieldNameWithEqualSign) && b.startsWith(fieldNameWithEqualSign)) {
        return false // b goes to the end
      } else {
        return a > b // lexicographical sorting (default)
      }
    })
    return pairStr.join("&")
  }

  var idToSelectorOrNull = function(x) {
    return (!!x) ? '#' + x : null;
  }

  $.fn.dashdo = function(options) {
    var that = $(this);
    var settings = $.extend({
      // These are the defaults.
      uuidUrl: 'uuid',
      uuidInterval: 10000,

      colorSelected: '#1F77B4',
      colorUnSelected: '#A5C8E1',

      containerSelector: idToSelectorOrNull($(this).attr('id')),
      switcherElements: null,
      switcherAttr: 'href',
      switcherEvent: 'click',

      periodicSubmitSelector: '.dashdo-periodic-submit',
      dashdoTitleSelector: '#dashdo-title',

      multiselectNamesSelector: '.dashdo-plotly-multi-select-names',
      resetLinkSelector: '.dashdo-resetlink',
    }, options)

    var setFormActive = function(b) {
      $(':input').prop('readonly', !b)
    }

    $(this).on('change', ':input', function(event) {
      $(this).filter('form').attr('data-last-changed-field', event.currentTarget.name)
      if (typeof(manual_submit) === 'undefined' || !manual_submit) {
        setFormActive(false)
        resubmit()
      }
    }.bind(this))

    this.filter('form').on('submit', function(e) {
      e.preventDefault()  // no 'native' submitting
    })

    var requestHtmlFromServer = function(url, data, onSuccess) {
      $.ajax({
        type: 'POST',
        url: url,
        data: data,
        contentType: 'application/x-www-form-urlencoded; charset=UTF-8',
        success: onSuccess,
      })
    }

    var restyleAndSetClickHandlers = function() {
      $('[data-dashdo-action]').each(function(i, e) {
        $(e).click(function(){
          var actNm = $(this).attr("data-dashdo-action");
          var url = that.attr('action')+"/action/"+actNm
          resubmit(url);
        })
      })
      $('[data-dashdo-redirect]').each(function(i, e) {
        location.href = $(this).attr("data-dashdo-redirect");
      })
      $(".dashdo-show-loading").hide();
      $('.dashdo-plotly-select .js-plotly-plot').each(function() {  // TODO what if there is no graph?
        var graphData = this.data[0]
        var axis = (graphData.orientation === 'h') ? 'y' : 'x'
        var values = $(this).siblings('input[name]').map(function() {  // name must be specified!
          return this.value
        }).get()

        var restyle = function() {
          if (values.length !== 0) {
            var os
            var colorContainer
            var colorContainerKey
            switch(graphData.type) {
              case 'bar':
                os = graphData[axis].map(function(p) {  // example: graphData['y']
                  return (values.indexOf(p) !== -1) ? // p `elem` values == True
                    settings.colorSelected :
                    settings.colorUnSelected
                })

                Plotly.restyle(this, {'marker.color' : [os]}, [0])
                return  // we returning - barplot has nothing to do with the cycle below!

              case 'pie':
                colorContainer = graphData.marker.colors
                colorContainerKey = 'marker.colors'
                break

              case 'scatter':
                colorContainer = graphData.marker.color
                colorContainerKey = 'marker.color'
                break
            }

            os = []
            for (var k = 0; k < colorContainer.length; k++) {
              if(values.indexOf(graphData.customdata[k]) === -1) { // TODO: if no customdata?
                os.push(shadeBlendConvert(0.7, colorContainer[k])) // if not selected, it is lighter
              } else {
                os.push(colorContainer[k]) // selected, leave it 'as is'
              }
            }
            var plotlyRestyleOpts = {}
            plotlyRestyleOpts[colorContainerKey] = [os]
            Plotly.restyle(this, plotlyRestyleOpts, [0])
          }
        }.bind(this)
        restyle()

        if(!!settings.resetLinkSelector) {
          $(settings.resetLinkSelector).each(function() {
            $(this).on('click', function() {
              $(this).siblings('input[name]').remove()
              resubmit()
            })
          })
        }

        var currentMultipleFieldName = $(this).siblings(settings.multiselectNamesSelector).val()
        var isMultiple = !!currentMultipleFieldName

        $(this).get(0).on('plotly_click', function(data) {
          var selectedValueFromPlot
          switch(graphData.type) {
            case 'bar':
              selectedValueFromPlot = data.points[0][axis]
              break
            case 'pie':
              selectedValueFromPlot = ('customdata' in graphData) ?
                graphData.customdata[data.points[0].i] :  // TODO: if no customdata?
                graphData.values[data.points[0].i]
              break
            default:
              selectedValueFromPlot = ('customdata' in data.points[0]) ? // TODO: if no customdata?
                data.points[0].customdata :
                data.points[0].pointNumber
          }

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

          resubmit()
        }.bind(this))
      })
    }
    restyleAndSetClickHandlers()

    var resubmit = function(url) {
      if(!!settings.containerSelector) {
        var whatToSend = sortSerializedString($(this).serialize(), $(this).attr('data-last-changed-field'))
        url = typeof url !== 'undefined' ? url : $(this).attr('action');
        $(".dashdo-show-loading").show();
        requestHtmlFromServer(
          url,
          $(this).serialize(),
          function(data) {
            var incomingDOM = $.parseHTML(data, null, true)

            // select the same container from incomingHTML
            var containerLikeCurrent = $(incomingDOM).children(settings.containerSelector)

            // if there is such container, replace current container with its contents
            // if no container found, then place all the incomingHTML into the container
            // useful for partial rendering of folder in rdashdo
            var bigDOMorSmallContainer =
              (containerLikeCurrent.length > 0) ?
                $(containerLikeCurrent).contents() :
                incomingDOM

            // cached things are to stay the same (replace with values from old DOM)
            $(bigDOMorSmallContainer)
              .children('.dashdo-cashed-not-changed')
              .each(function() {
                var parent = $(this).parent()
                var fieldName  = parent.data('dashdo-cashed')
                var cachedContents = $("[data-dashdo-cashed='" + fieldName + "']").contents()
                $(parent).html(cachedContents)
              })

            $(settings.containerSelector).html(bigDOMorSmallContainer)

            restyleAndSetClickHandlers()
            // if switched & rendered successfully, renew periodic submit loop
            clearInterval(submitTimer)
            periodicSubmitLoop()
            setFormActive(true)
          }.bind(this)
        )
      }
    }.bind(this)

    var uuid = null
    var uuidLoop = function() {
      $.get(settings.uuidUrl).done(function(data) {
        if(uuid && uuid != data) {
          resubmit()
        }
        uuid = data
      }).always(function() {
        setTimeout(uuidLoop, Math.max(settings.uuidInterval, 1000));
      })
    }
    if(settings.uuidInterval>0)
      uuidLoop();

    var submitTimer = null
    var periodicSubmitLoop = function() {
      var currentPeriodicSubmitValue = parseInt($(this).find(settings.periodicSubmitSelector).val())
      if(!!currentPeriodicSubmitValue) {
        resubmit()
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
      resubmit()
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
