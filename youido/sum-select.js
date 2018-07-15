
var youidoSumDummyPathPrefix = "dummy."; // prefix of paths in all invisible ctor subforms
var youidoSumDummyIdPrefix = "youido-sum-dummy-"; // for each invisible ctor subform
var youidoSumRealSubformIdPrefix = "youido-sum-real-subform-"; // for the visible ctor subform
var youidoSumRealContainerIdPrefix = "youido-sum-real-container-"; // for div around subform
var youidoSumChoiceIdPrefix = "youido-sums."; // prefix of elem id for invisible choice input

/*
   Each constructor's subform is stored in an invisible div, the
   digestive-functor paths of which are all prefixed with 'dummy.'. When
   the user selects a constructor option, we clone the appropriate div,
   make it visible, and strip the dummy prefix from all attributes.
 */
function youidoStripDummyPrefix(prefix, elem) {
  var attrs = ['for', 'id', 'name'];
  for (var i=0; i < attrs.length; i++) {
    $(elem).find("*[" + attrs[i] + "^='" + prefix + "']")
	   .attr(attrs[i], function(i,old) { return old.slice(prefix.length)});
  }
}

function youidoSelectConstructor(elem, viewName, viewCtx, fieldName) {
  var ctor = elem.childNodes[elem.selectedIndex].text;
  var fieldRefNoName = (viewCtx == '' ? '' : viewCtx + '.') + fieldName;
  var fieldRef = viewName + '.' + fieldRefNoName;
  var dummyId = youidoSumDummyIdPrefix + fieldRef + '.' + ctor;
  var dummy = document.getElementById(dummyId);
  var real = dummy.cloneNode(true);
  var container = document.getElementById(youidoSumRealContainerIdPrefix + fieldRef);
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }
  real.removeAttribute('id');
  real.style.display = 'inherit';
  youidoStripDummyPrefix(youidoSumDummyPathPrefix, real);

  var choiceId = youidoSumChoiceIdPrefix + fieldRefNoName;
  var choice = document.getElementById(choiceId);
  choice.value = ctor;

  if (real.className.indexOf("youido-u1-container") != -1) {
    container.style.display = 'none';
  } else {
    container.style.display = 'inherit';
  }
  container.insertBefore(real, null);
  return false;
}
