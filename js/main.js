let isDarkMode = false;

function setDarkMode(darkMode){
    document.body.className = darkMode ? "dark" : "";

    localStorage.setItem("color-scheme", darkMode ? "dark" : "light")
}

function toggleDarkMode(){
    isDarkMode = !isDarkMode;

    setDarkMode(isDarkMode)
}

isDarkMode = localStorage.getItem("color-scheme") == "dark"
setDarkMode(isDarkMode)

