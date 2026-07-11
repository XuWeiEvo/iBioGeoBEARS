#' Open the ordinary-user quick-start guide
#'
#' Opens or returns the installed Markdown guide for first-time users. English
#' and simplified Chinese guides are included. The guide covers installation
#' checks, the bundled example project, the Shiny runner, result bundles,
#' diagnostic bundles, and common recovery steps.
#'
#' @param browse Logical. If `TRUE`, open the guide with [utils::browseURL()].
#'   Defaults to `interactive()`.
#' @param language Character. Guide language, either `"en"`, `"zh-CN"`, or
#'   `"zh"`. Defaults to `"en"`.
#' @return The absolute path to the installed guide.
#' @export
open_user_guide <- function(browse = interactive(), language = c("en", "zh-CN", "zh")) {
  language <- match.arg(language)
  guide_file <- if (language %in% c("zh-CN", "zh")) {
    "ordinary-user-quick-start.zh-CN.md"
  } else {
    "ordinary-user-quick-start.md"
  }

  guide <- system.file(
    "docs",
    guide_file,
    package = "iBiogeobears",
    mustWork = FALSE
  )
  if (!nzchar(guide) || !file.exists(guide)) {
    stop(
      "The requested iBiogeobears ordinary-user guide was not found in the installed package.",
      call. = FALSE
    )
  }

  guide <- as_path(guide)
  if (isTRUE(browse)) {
    utils::browseURL(guide)
  }
  guide
}
