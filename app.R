options(box.path = getwd())
options(browser = function(url) {
	# Launch Google Chrome in a new window without blocking R
	system2("/usr/bin/google-chrome", c("--new-window", url), wait = FALSE)
})
options(shiny.port = 8000)
# Rhino / shinyApp entrypoint. Do not edit.
# Workaround: rhino::app() calls utils::browseURL() directly and it
# requires options("browser") to be a non-empty string. Some startup
# code may clear that option; temporarily override utils::browseURL
# to launch Google Chrome directly, then restore it after the app exits.
{
	ns_utils <- asNamespace("utils")
	old_browse <- get("browseURL", envir = ns_utils)
	unlockBinding("browseURL", ns_utils)
	assign(
		"browseURL",
		function(url, browser = getOption("browser"), encodeIfNeeded = FALSE) {
			system2("/usr/bin/google-chrome", c("--new-window", url), wait = FALSE)
		},
		envir = ns_utils
	)
	lockBinding("browseURL", ns_utils)

	on.exit({
		unlockBinding("browseURL", ns_utils)
		assign("browseURL", old_browse, envir = ns_utils)
		lockBinding("browseURL", ns_utils)
	}, add = TRUE)

	rhino::app()
}
