library(tidyverse)

texts <- "chökta bonin klo"
lexicon <- list(
  list(lex = "chuk"),
  list(lex = "-ta"),
  list(lex = "bon"),
  list(lex = "-i"),
  list(lex = "-n"),
  list(lex = "klo")
)

tok <- str_split(texts, " ")

detected <- map(lexicon, ~str_detect(.x$lex, "klo"))

lexicon[unlist(detected)]

word <- "chökta"

lexicon <- list(
  list(
    lemma = "chuk",
    morph_type = "root",
    allomorphs = list(
      list(
        spelling = "chuk"
      ),
      list(
        spelling = "chök"
      )
    )
  ),
  list(
    lemma = "-ta",
    morph_type = "suffix",
    allomorphs = list(
      list(
        spelling = "ta"
      )
    )
  )
)

lex <- yaml::read_yaml("parsr/lexicon.yaml")

morphs <- map(lex, function(x) {
  map(x$allomorphs, function(y) {
      y$morph
    }
  )
}
) %>%
  unlist()

new_morphs <- str_split(morphs, "")
splitted <- str_split("chekta", "")[[1]]
j <- 0

for (i in 1:length(splitted)) {
  cat(i, "..........\n")
  if (length(new_morphs) > 0) {
    j <- j + 1
    new_morphs <- new_morphs[unlist(map(new_morphs, ~str_detect(.x[j], splitted[i])))]
    print(new_morphs)
    if (length(new_morphs) == 1) {
      cat("It's a match!\n")
    } else {
      next
    }
  } else if (length(new_morphs) == 0) {
    if (i < length(splitted)) {
      cat("Parsing incomplete: ", splitted[1:i], "\n")
      new_morphs <- str_split(morphs, "")
      j <- 1
      next
    } else {
      cat("End of string\n")
      break
    }
  }
}

new_morphs <- str_split(morphs, "")
splitted <- str_split("chökta", "")[[1]]
# splitted <- str_split("chökta", "")[[1]]
j <- 0
i <- 0
match <- FALSE

while (i < length(splitted)) {
  i <- i + 1
  j <- j + 1
  cat(i, "..........\n")
  cat("Parsing <", splitted[1:i], "\n")
  new_morphs <- new_morphs[unlist(map(new_morphs, ~str_detect(.x[j], splitted[i])))]
  cat("Current:\n")
  print(new_morphs)

  if (length(new_morphs) > 1) {
    next
  } else if (length(new_morphs) == 1 & !is.null(new_morphs[[1]])) {
    cat("It's a 1\n")
    if (i == length(splitted)) {
      cat("It's a full match!\n")
      break
    } else {
      match <- TRUE
      matched <- new_morphs
      print(matched)
      next
    }
  } else if (length(new_morphs) == 0 | is.null(new_morphs[[1]])) {
    cat("It's a 0\n")
    if (match) {
      match <- FALSE
      j <- 0
      if (i == length(splitted)) {
        break
      } else {
        i <- i - 1
        print(i)
        new_morphs <- str_split(morphs, "")
        print(new_morphs)
        next
      }
    } else {
      match <- FALSE
      j <- 0
      i <- i - 1
      new_morphs <- str_split(morphs, "")
      next
    }
  }
}
