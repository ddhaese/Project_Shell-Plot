// Function carrying out the completion of the template
const complete = (template, data) => {
  for (var item in data) {
    template = template.replace(RegExp('{{' + item + '}}', 'g'), data[item]);
  }

  template = template.replace(/{\w+}/g, '');

  return template;
}

// Function to be executed upon mouse entering one of the source elements
const onEpisodeMouseEnter = (e) => {
  const targetName = e.target.closest("svg").dataset.infoboxTarget;
  const templateName = e.target.closest("svg").dataset.infoboxTemplate;

  target = document.getElementById(targetName);
  template = document.getElementById(templateName).innerHTML;

  data = { Subject: e.target.dataset.subjectName };
  target.innerHTML = complete(template, data);
}

// Function to be executed upon mouse leaving one of the source elements
const onEpisodeMouseLeave = (e) => {
  const targetName = e.target.closest("svg").dataset.infoboxTarget;

  target = document.getElementById(targetName);

  target.innerHTML = "";
}

// Function to be executed upon clicking one of the source elements
const onEpisodeMouseClick = (e) => {
  data = { Subject: e.target.dataset.subjectName };
  
  var type = "text/plain";
  var blob = new Blob([JSON.stringify(data)], { type });
  var clipData = [new ClipboardItem({ [type]: blob })];

  navigator.clipboard.write(clipData)
}

const onDocumentReady = () => {
  document.querySelectorAll(".episode").
    forEach(el => {
      el.addEventListener("mouseenter", onEpisodeMouseEnter);
      el.addEventListener("mouseleave", onEpisodeMouseLeave);
      el.addEventListener("click", onEpisodeMouseClick)
    });
}

// Binding the source element to the action function when document is ready
document.addEventListener('DOMContentLoaded', onDocumentReady);
