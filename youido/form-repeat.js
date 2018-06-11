var youidoListClass = 'youido_multi_list';
var youidoItemClass = 'youido_multi_item';

function youidoAncestor(button, cls) {
  var parent = $(button).parent();
  while (!parent.hasClass(cls)) {
    parent = $(parent).parent();
  }
  return parent;
}

function youidoReplaceIndex(currentPath, pathRegexp, idx) {
  return currentPath.replace(pathRegexp, '$1.' + idx);
}

function youidoUpdatePaths($items, multiField, initPath) {
  var regex = new RegExp('^(' + initPath + ')\\.\\d+');
  $items.each(function (idx) {
    $(this).find("*[for^='" + initPath + ".']").attr('for', function(i,old) {
      return youidoReplaceIndex(old, regex, idx);
    });
    $(this).find("*[id*='" + initPath + ".']").attr('id', function(i,old) {
      return youidoReplaceIndex(old, regex, idx);
    });
    $(this).find("*[name*='" + initPath + ".']").attr('name', function(i,old) {
      return youidoReplaceIndex(old, regex, idx);
    });
  });
}

function youidoUpdateIndices(initPath, newLength) {
  var newVal = '';
  if (newLength <= 1) {
    newVal = '0';
  } else {
    for (var i=0; i < newLength; i++) {
      newVal = newVal + i;
      if (i < newLength - 1) {
	newVal = newVal + ',';
      }
    }
  }
  var indicesId = initPath + '.indices';
  var $indices = $(document.getElementById(indicesId));
  $indices.val(newVal);
  $indices.attr('value', newVal);
}

function youidoAddItem(button, multiField, initPath) {
  var $thisItem = $(youidoAncestor(button, youidoItemClass));
  var newItem = $thisItem.clone(true);
  $thisItem.after(newItem);

  var $items = $(youidoAncestor(button, youidoListClass)).children('div');
  youidoUpdatePaths($items, multiField, initPath);
  youidoUpdateIndices(initPath, $items.length);
}

function youidoRemoveItem(button, multiField, initPath) {
  var $thisItem = $(youidoAncestor(button, youidoItemClass));
  var $itemsContainer = $(youidoAncestor(button, youidoListClass));
  $thisItem.remove();
  var $items = $itemsContainer.children('div');
  youidoUpdatePaths($items, multiField, initPath);
  youidoUpdateIndices(initPath, $items.length);
}
