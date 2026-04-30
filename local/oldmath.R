#' Convert math-enabled markdown to HTML
#'
#' Note, this uses the ketex package. It in turn requires V8, which is 12 MB
#' or so. Prefer using shiny's built-in withMathJax().
#'
#' @export
md = function(text)
{
    # Collapse text into a single string
    text = paste0(text, collapse = "")

    # Replace all math entries with placeholders that will survive markdown
    math_pattern = "(?<!\\\\)(?:\\\\{2})*\\$(.*?)(?<!\\\\)(?:\\\\{2})*\\$"
    maths = list()
    replace_math = function(math) {
        maths <<- c(maths, stringr::str_sub(math, 2, -2))
        paste0('<span class="tweak-math">', length(maths), '</span>')
    }
    text = stringr::str_replace_all(text, math_pattern, replace_math)

    # Convert text with markdown to HTML; trim if single paragraph
    text = commonmark::markdown_html(text, extensions = TRUE)
    text = stringr::str_trim(text)
    if (stringr::str_count(text, "</p>") == 1 &&
        stringr::str_sub(text, 1, 3) == "<p>" && stringr::str_sub(text, -4, -1) == "</p>") {
        text = stringr::str_sub(text, 4, -5)
    }

    # Convert LaTeX math to HTML
    maths = lapply(maths, katex::katex_html, displayMode = FALSE, preview = FALSE)

    # Insert HTML math into text
    span_pattern = '<span class="tweak-math">([0-9]+)</span>'
    replace_span = function(span) {
        idx = as.numeric(stringr::str_match(span, span_pattern)[1,2])
        maths[[idx]]
    }

    shiny::HTML(stringr::str_replace_all(text, span_pattern, replace_span))
}
