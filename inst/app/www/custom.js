$(function() {
  $("#var_list").on("click", "button", function() {
    $("#var_list button").removeClass("btn-primary");
    $(this).addClass("btn-primary");
    // set to null first to register same item click
    Shiny.setInputValue("var_selected", null);
    Shiny.setInputValue("var_selected", $(this).text());
  });

  $('#level_list_display').on("change", 'input',
    function() {
      Shiny.setInputValue("level_list_update", null);
      Shiny.setInputValue("level_list_update", 1);
  });

  closeBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (!box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  openBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  $('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box')
           .find('[data-widget=collapse]')
           .click();
  });

});
