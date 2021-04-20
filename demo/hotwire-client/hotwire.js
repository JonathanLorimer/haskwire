import * as Turbo from "@hotwired/turbo"

window.Turbo = Turbo

document.addEventListener("turbo:load", function(e) {
  console.group()
  console.log("turbo:load")
  console.log(e)
  console.groupEnd()
})


