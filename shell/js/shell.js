var scaleFactor = 1.1;

// Function carrying out the completion of the template
shellComplete = (template, data, headers) => {
  data = data.split("|");
  headers = headers.split("|")

  for (let i = 0; i < headers.length; i++) {
    dataItem = data[i];
    header = headers[i];
    template = template.replace(RegExp("{{" + header + "}}", "g"), dataItem);
  }

  // template = template.replace(/{\w+}/g, "");

  return template;
}

// Function to be executed upon mouse entering one of the source elements
shellOnEpisodeMouseEnter = (e) => {
  const data = e.target.dataset.episodeDetails;
  const parent = e.target.closest("div.shell");
  const targetclass = parent.dataset.infoboxClass;
  const templateclass = parent.dataset.infoboxTemplate;
  const headers = parent.dataset.episodeDetailsHeaders;
  const observation = document.getElementById(e.target.dataset.observation);

  e.target.classList.add("highlight");
  observation.classList.add("highlight");

  trg = parent.querySelector("." + targetclass);
  template = parent.querySelector("." + templateclass).innerHTML;
  
  trg.innerHTML = shellComplete(template, data, headers);
}

// Function to be executed upon mouse leaving one of the source elements
shellOnEpisodeMouseLeave = (e) => {
  const parent = e.target.closest("div.shell");
  const observation = document.getElementById(e.target.dataset.observation);
  const targetclass = parent.dataset.infoboxClass;

  e.target.classList.remove("highlight");
  observation.classList.remove("highlight");

  trg = parent.querySelector("." + targetclass);

  trg.innerHTML = "";
}

// Function to be executed upon clicking one of the source elements
shellOnEpisodeMouseClick = (e) => {
  const parent = e.target.closest("div.shell");
  const headers = parent.dataset.episodeDetailsHeaders.split("|").join("\t");
  const data = e.target.dataset.episodeDetails.split("|").join("\t");
  
  var type = "text/plain";
  var blob = new Blob([headers + "\n" + data], { type });
  var clipData = [new ClipboardItem({ [type]: blob })];

  navigator.clipboard.write(clipData)
}

// Function to be executed upon zooming
shellOnZoom = (e) => {
  e.preventDefault();

  let target;

  if (e.target.nodeName == "svg") {
    target = e.target;
  } else {
    target = e.target.closest("svg");
  }
  
  var shellCursor = target.createSVGPoint();
  
  function cursorPoint(evt){
    shellCursor.x = evt.clientX; shellCursor.y = evt.clientY;
    return shellCursor.matrixTransform(target.getScreenCTM().inverse());
  }

  let {x, y, width, height} = target.viewBox.baseVal;
  const scale = Math.pow(scaleFactor, Math.sign(e.deltaY));

  const loc = cursorPoint(e);

  const widthN = width * scale;
  const heightN = height * scale;
  const xN = x + (loc.x - x) / width * (width - widthN);
  const yN = y + (loc.y - y) / height * (height - heightN);

  target.setAttribute("viewBox", [xN, yN, widthN, heightN].join(" "));
}

shellOnDocumentReady = () => {
  document.querySelectorAll("g.shell-episodes path").
    forEach(el => {
      el.addEventListener("mouseenter", shellOnEpisodeMouseEnter);
      el.addEventListener("mouseleave", shellOnEpisodeMouseLeave);
      el.addEventListener("click", shellOnEpisodeMouseClick);
    });
    
  document.querySelectorAll("div.shell div.shell-plot svg").
    forEach(el => {
      el.addEventListener("wheel", shellOnZoom);
  });
}

// Binding the source element to the action function when document is ready
document.addEventListener("DOMContentLoaded", shellOnDocumentReady);

