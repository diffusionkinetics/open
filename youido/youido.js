// List-related constants
var youidoItemClass = 'youido_multi_item';
var youidoDummyItem = 'youido_dummy_item';

// Sum-related constants
var youidoSumDummyPathPrefix = "dummy."; // prefix of paths in all invisible ctor subforms
var youidoSumDummyIdPrefix = "youido-sum-dummy-"; // for each invisible ctor subform
var youidoSumRealSubformIdPrefix = "youido-sum-real-subform-"; // for the visible ctor subform
var youidoSumRealContainerIdPrefix = "youido-sum-real-container-"; // for div around subform
var youidoSumChoiceIdPrefix = "youido-sums."; // prefix of elem id for invisible choice input
var youidoSumPrefixes = [youidoSumDummyPathPrefix,
			 youidoSumDummyIdPrefix,
			 youidoSumRealSubformIdPrefix,
			 youidoSumRealContainerIdPrefix,
			 youidoSumChoiceIdPrefix];

//
// List forms
//

function youidoReplaceIndex(currentPath, pathRegexp, idx) {
  return currentPath.replace(pathRegexp, '$1.' + idx);
}

function youidoUpdatePaths($items, fieldName, fieldPath) {
  // regex matches e.g. form.fieldName.0 and form.fieldName.-1
  var attrs = ['for', 'id', 'name'];
  var prefixes = [fieldPath];
  for (var i=0; i < youidoSumPrefixes.length; i++) {
    prefixes.push(youidoSumPrefixes[i] + fieldPath);
  }
  // the sum choice invisible input does not include the top level form name:
  prefixes.push(youidoSumChoiceIdPrefix + fieldPath.replace(/^.+?\./, ''));

  $items.each(function (idx) {
    for (var j=0; j < attrs.length; j++) {
      for (var k=0; k < prefixes.length; k++) {
	var regex = new RegExp('^(' + prefixes[k] + ')\\.-?\\d+');
	$(this).find("*[" + attrs[j] + "^='" + prefixes[k] + ".']")
	       .attr(attrs[j], function(i,old) {
		 return youidoReplaceIndex(old, regex, idx);
	       });
      }
    }
  });
}

function youidoGetFieldPath(itemsDiv) {
  var indices = $(itemsDiv).children("input[id$='.indices']")[0];
  if (!!indices) {
    return indices.id.replace(/\.indices$/, '');
  } else return null;
}

function youidoUpdateIndices(fieldPath, newLength) {
  var newVal = '';
  for (var i=0; i < newLength; i++) {
    newVal = newVal + i;
    if (i < newLength - 1) {
      newVal = newVal + ',';
    }
  }
  var indices = document.getElementById(fieldPath + '.indices');
  indices.setAttribute('value', newVal);
}

function youidoUpdate($items, fieldName, fieldPath) {
  var dummySel = "[id='" + fieldPath + '.' + youidoDummyItem + "']";
  var $itemsNoDummy = $items.not(dummySel);
  youidoUpdatePaths($itemsNoDummy, fieldName, fieldPath);
  youidoUpdateIndices(fieldPath, $itemsNoDummy.length);
}

function youidoAddItem(itemsDiv, fieldName) {
  var fieldPath = youidoGetFieldPath(itemsDiv);
  var dummyId = fieldPath + '.' + youidoDummyItem;
  var dummy = document.getElementById(dummyId);
  var newItem = dummy.cloneNode(true);
  newItem.setAttribute('style', 'display: inherit');
  newItem.setAttribute('id', newItem.getAttribute('id').replace(dummyId, ''));
  var $items = $(itemsDiv).children('div.' + youidoItemClass);
  $items[$items.length - 1].after(newItem);
  $items.push(newItem);
  youidoUpdate($items, fieldName, fieldPath);
}

function youidoRemoveItem(item, fieldName) {
  var itemsDiv = item.parentNode;
  var fieldPath = youidoGetFieldPath(itemsDiv);
  itemsDiv.removeChild(item);
  youidoUpdate($(itemsDiv).children('div.' + youidoItemClass), fieldName, fieldPath);
}

//
// ** Sum forms
//

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
	  .attr(attrs[i], function(i,old) { return old.slice(prefix.length); });
  }
}

function youidoSelectConstructor(elem, viewName, fieldName) {
  var ctor = elem.childNodes[elem.selectedIndex].text;
  var fieldRef = elem.parentNode.firstChild.getAttribute('for');
  var fieldRefNoName = fieldRef.replace(viewName + '.', '');
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
