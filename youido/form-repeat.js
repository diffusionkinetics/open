var youidoListClass = 'youido_multi_list';
var youidoItemClass = 'youido_multi_item';
var youidoDummyItem = 'youido_dummy_item';
var youidoAddLoneItemClass = 'youido_add_lone_item';

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

function youidoAddItem(item, fieldName, fieldPath) {
  var $thisItem = $(item);
  var newItem = $thisItem.clone(true);
  $thisItem.after(newItem);

  var $items = $thisItem.parent().children('div');
  youidoUpdate($items, fieldName, fieldPath);

  // Hide add item button if transitioning from 0 to 1 items (1 to 2 incl. dummy)
  if ($items.length == 2) {
    document
      .getElementById(fieldName + '.' + youidoAddLoneItemClass)
      .setAttribute('style', 'display: none');
  }
}

function youidoAddLoneItem(itemsDiv, fieldName, fieldPath) {
  var dummyId = fieldPath + '.' + youidoDummyItem;
  var dummy = document.getElementById(dummyId);
  var newItem = dummy.cloneNode(true);
  newItem.setAttribute('style', 'display: inherit');
  newItem.setAttribute('id', newItem.getAttribute('id').replace(dummyId, ''));

  document
    .getElementById(fieldPath + '.' + youidoAddLoneItemClass)
    .setAttribute('style', 'display: none');
  itemsDiv.appendChild(newItem);
  youidoUpdate($(itemsDiv).children('div'), fieldName, fieldPath);
}

function youidoRemoveItem(item, fieldName, fieldPath) {
  var itemsDiv = item.parentNode;
  itemsDiv.removeChild(item);
  var $items = $(itemsDiv).children('div');
  youidoUpdate($items, fieldName, fieldPath);

  // Display add item button when new length is 0 (== 1 incl. dummy)
  if ($items.length === 1) {
    document
      .getElementById(fieldPath + '.' + youidoAddLoneItemClass)
      .setAttribute('style', 'display: inherit');
  }
}
