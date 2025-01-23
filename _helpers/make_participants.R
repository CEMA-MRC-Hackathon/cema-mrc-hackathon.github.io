# 1. Copy and clean picture names


# 2. Identify new qmd based on photo names

# get new headshots
l <- list.files("participants/headshots/")
p <- tools::file_path_sans_ext(l)

# get current qmd
l2 <- grep("qmd", list.files("participants/"), value = TRUE)
p2 <- tools::file_path_sans_ext(l2)

# find diff
new <- setdiff(p, p2)
to_add <- paste0(
  "headshots/",
  list.files("participants/headshots/",
             pattern = paste0(new, collapse = "|"))
)

# 3. Fill in info function:
make_qmd <- function(headshot,
                     bio,
                     affiliation = "Centre for Epidemiological Modelling and Analysis (CEMA)",
                     name = NULL,
                     twitter = NULL,
                     linkedin = NULL,
                     googlescholar = NULL,
                     personal = NULL,
                     github = NULL,
                     role = "Participant") {

  # fetch name if not file name
  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(headshot))
    name <- stringr::str_to_title(gsub("-", " ", name))
  }

  # clean headshot
  headshot <- gsub("(^.*)(headshots/*)", "\\2", headshot)

  # create first set of page info
  ll <- list(
    "header" = "---",
    "name" = paste0('title: \"', name, '\"'),
    "aff" = paste0('subtitle: \"', affiliation, '\"'),
    "image" = paste0('image: ', headshot),
    "category_head" = "categories:",
    "categories" = paste0("  - ", role),
    "about" = "about:",
    "template" = "  template: jolla",
    "links" = "  links:"
  )

  # create the link section
  make_link_three <- function(icon, text, href) {
    c(paste0("    - icon: ", icon),
      paste0("      text: ", text),
      paste0("      href: ", href))
  }

  # parse the link
  parse_link <- function(x, name = deparse(substitute(x))) {
    if(is.null(x) || is.na(x)) {
      return(NULL)
    } else {
        if (name == "googlescholar") name <- "Google Scholar"
        if (name == "personal") name <- "Personal Website"
      return(
        make_link_three(icon = name, text = stringr::str_to_title(name), href = x)
      )
    }
  }

  # make the link list
  ll2 <- c(
    parse_link(linkedin),
    parse_link(twitter),
    parse_link(googlescholar),
    parse_link(personal),
    parse_link(github)
  )
  if (is.null(ll2)) {
    ll$links <- NULL
  }

  # make the end list
  ll3 <- list(
    "header" = "---",
    "blank" = "",
    "bio" = bio
  )

  return(c(as.character(ll), ll2, as.character(ll3)))

}

# 4. Organising Committee
pages <- list("jd" = make_qmd(headshot = to_add[1], role = "Organiser",
                              bio = "Jeanette Dawa (MBChB, MSc, PhD) is a public health specialist/epidemiologist with a background in medicine, public health and infectious disease modelling.",
                              googlescholar = "https://scholar.google.com/citations?user=L1I0WJkAAAAJ&hl=en",
                              linkedin = "https://www.linkedin.com/in/jeanette-dawa/"),
              "mn" = make_qmd(headshot = to_add[3], role = "Organiser",
                              bio = "Mutono is a data scientist currently using applied epidemiological modelling techniques to provide evidence for control and elimination of Neglected Tropical Diseases.",
                              googlescholar = "https://scholar.google.com/citations?user=DE4gTQYAAAAJ&hl=en"),
              "ts" = make_qmd(headshot = to_add[4], role = "Organiser",
                              bio = "Triza Shigoli is a Project Management and Organizational Development professional, and the Administrative Manager at the Center for Epidemiological Modelling and Analysis (CEMA), with experience in supporting the delivery of health research projects in collaboration with a multidisciplinary team of scientists and technical experts.",
                              linkedin = "https://www.linkedin.com/in/triza-shigoli/"),
              "wm" = make_qmd(headshot = to_add[5], role = "Organiser",
                              bio = "Waithera is an Administration Officer at CEMA, dedicated to ensuring smooth operations and effective coordination for trainings that take place at the organisation.",
                              linkedin = "https://www.linkedin.com/in/waitherawmwangi/")
)
names(pages) <- paste0("participants/", new, ".qmd")

lapply(seq_along(pages), function(x){writeLines(text = pages[[x]], con = names(pages[x]))})
