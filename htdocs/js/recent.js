function getTagSearchResults (tag) {
  $.ajax({
    url: "/j/t/" + tag,
    success: function (data) {
      $("div#tag_list_pastes").html(data);
      rebindTagLinks();
    }
  });
}

function rebindTagLinks () {
  $("a.descTag").click(function (event) {
    event.preventDefault();
    var tag = $(this).html().substr(1);
    getTagSearchResults(tag);
    $("form#tag_search_form input[type=text]").val(tag);
  });
}

$("document").ready(function () {

  $("form#tag_search_form").submit(function () {
    var tag = $("form#tag_search_form input[type=text]").val();
    getTagSearchResults(tag);
    return false;
  });

  rebindTagLinks();

});
