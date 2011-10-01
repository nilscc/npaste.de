// Show a text in an input field and remove it on focus
function inputDescription(el,str) {
  $(el).val(str);
  $(el).addClass("inputDesc");
  $(el).focus(function () {
    if ($(this).val() == str) {
      $(this).val("");
      $(this).removeClass("inputDesc");
    }
  });
}

$(document).ready(function () {

  inputDescription("input#desc", "Enter description…");

});
