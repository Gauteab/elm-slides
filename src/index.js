var storedData = localStorage.getItem("model");
var flags = storedData ? JSON.parse(storedData) : null;

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: flags,
});

app.ports.setStorage.subscribe((state) => {
  localStorage.setItem("model", JSON.stringify(state));
});
