function fixAllTitles() {

  function updatePicker(id, allLabel) {
    var sel = $('#' + id);
    if (sel.length === 0) return;

    var total = sel.find('option').length;
    var selected = sel.val() ? sel.val().length : 0;

    var button = sel.parent().find('.filter-option-inner-inner');
    if (selected === total) {
      button.text(allLabel);
    }
  }

  $(document).on('changed.bs.select loaded.bs.select refreshed.bs.select', function() {
    updatePicker('positions', 'All Positions');
    updatePicker('teams', 'All Teams');
    updatePicker('years', '2000 - Present');
  });
}

$(document).ready(fixAllTitles);
