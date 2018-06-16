var youidoItemClass = 'youido_multi_item';
var youidoDummyItem = 'youido_dummy_item';

function youidoReplaceIndex(currentPath, pathRegexp, idx) {
  return currentPath.replace(pathRegexp, '$1.' + idx);
}

function youidoUpdatePaths($items, fieldName, fieldPath) {
  // regex matches e.g. form.fieldName.0 and form.fieldName.-1
  var regex = new RegExp('^(' + fieldPath + ')\\.-?\\d+');
  var attrs = ['for', 'id', 'name'];
  $items.each(function (idx) {
    for (var j=0; j < attrs.length; j++) {
      $(this).find("*[" + attrs[j] + "^='" + fieldPath + ".']")
	.attr(attrs[j], function(i,old) {
	  return youidoReplaceIndex(old, regex, idx);
	});
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
