function createCookie(name,value,days) {
  if (days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
  }
  else var expires = "";
  document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

function showNotification (name, text) {

  // See if the notification has already been shown
  var cookieVal = readCookie("notification-" + name);
  if (cookieVal == "hide") return;

  $("section#notifications").append(
    '<p id="notification-' + name + '">' + text + ' ' +
      '<a onclick="hideNotification(\'' + name + '\')">Hide</a>' +
    '</p>');

}

function hideNotification (name) {

  $("section#notifications p#notification-" + name).fadeOut();
  createCookie("notification-" + name, "hide", 10);

}

$(document).ready(function () {

//showNotification("08-01-12-downtime",     "npaste.de has some performance issues. Sorry for any downtimes!");
//showNotification("09-01-12-highlighting", "The highlighting engine is currently not working – sorry!");

});
