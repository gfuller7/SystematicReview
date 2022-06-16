library(tidyverse)
library(topicmodels)
library(tidytext)
library(textreadr)
library(pdftools)
library(SnowballC)

from.a.list.of.files.to.file.text.df = function(list.of.files)
{
  df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
  for (i in 1:length(list.of.files))
  {
    the.file = list.of.files[i]
    if (grepl(".pdf", the.file, ignore.case = TRUE) & file.exists(the.file)) {
      df.to.add = topic.model.from.pdf.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (is.valid.textreadr.file.format(the.file) & file.exists(the.file)) {
      df.to.add = topic.model.from.document.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (grepl(".pptx", the.file, ignore.case = TRUE) & file.exists(the.file)) {
      df.to.add = topic.model.from.pptx.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
    else if (file.exists(the.file)) { # default right now largely for html
      df.to.add = topic.model.from.html.to.tbl(the.file)
      df = df %>% rbind.data.frame(df.to.add)
    }
  }
  return(df)
}


from.file.text.df.to.tidytext = function(file.text.df, remove.numbers=TRUE, remove.stop.words=TRUE, stem.words=TRUE)
{
  file.text.df.tokens = unnest_tokens(file.text.df, output = word, input = text)
  if (remove.numbers)
  {
    # remove numbers
    file.text.df.tokens = filter(file.text.df.tokens, !str_detect(word, "^[0-9]*$"))
  }
  if (remove.stop.words)
  {
    # remove stop words
    file.text.df.tokens = anti_join(file.text.df.tokens, stop_words)
  }
  if (stem.words)
  {
    # stem the words
    file.text.df.tokens = mutate(file.text.df.tokens, word = wordStem(word))
  }
  
  
  file.text.df.tokens = file.text.df.tokens %>% count(document, word, sort = TRUE) %>% ungroup()
  return(file.text.df.tokens)
}

from.tidy.text.to.dtm = function(tidy.text.tbl)
{
  dtm.to.return = tidy.text.tbl %>% cast_dtm(document, word, n)
  return(dtm.to.return)
}

get.top.terms.from.topics = function(tidy.topics, top.n)
{
  top.terms.to.return = tidy.topics %>%  group_by(topic) %>% top_n(top.n, beta) %>% ungroup() %>% arrange(topic, -beta)
  return(top.terms.to.return)
}

is.valid.file.format = function(file.name)
{
  first.part = is.valid.textreadr.file.format(file.name)
  pptx.part = grepl(".pptx", file.name, ignore.case = TRUE)
  return (first.part | pptx.part)
}

is.valid.textreadr.file.format = function(file.name)
{
  if (grepl(".docx", file.name, ignore.case = TRUE) |
      grepl(".doc", file.name, ignore.case = TRUE)  |
      grepl(".rtf", file.name, ignore.case = TRUE)  |
      grepl(".txt", file.name, ignore.case = TRUE)  |
      grepl(".pdf", file.name, ignore.case = TRUE)  )
  {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

get.lda = function(doc.term.matrix, k.parm, control.parm=1234)
{
  lda.to.return = LDA(doc.term.matrix, k = k.parm, control = list(seed = control.parm))
  return(lda.to.return)
}

get.tidy.topics.from.lda = function(lda.model, per.document=FALSE)
{
  if (per.document == FALSE)
  {
    topics.to.return =  tidy(lda.model, matrix = "beta")
  }
  else
  {
    topics.to.return =  tidy(lda.model, matrix = "gamma")
  }
  return(topics.to.return)
}

get.tidy.document.classification.from.lda = function(tidy.topics.per.doc)
{
  topic.classifications.to.return = tidy.topics.per.doc %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup()
  
  return(topic.classifications.to.return)
}

topic.model.from.pdf.to.tbl = function(pdf.file)
{
  success = FALSE
  try({
    pdf_txt = pdf_text(pdf.file)
    pdf_txt = pdf_txt %>% str_squish() %>% str_replace_all(",","")
    pdf_txt = unlist(strsplit(pdf_txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    txt_this_iter = pdf_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(pdf.file, length(pdf_txt))
    df = tibble(cur_link, pdf_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
  
}

topic.model.from.pptx.to.tbl = function(pptx.file)
{
  success = FALSE
  try({
    pptx_obj = read_pptx(pptx.file)
    pptx_tbl = pptx_summary(pptx_obj)
    pptx_tbl = pptx_tbl %>% filter(nchar(text) > 3)
    pptx_txt = pptx_tbl$text
    txt_this_iter = pptx_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(pptx.file, length(pptx_txt))
    df = tibble(cur_link, pptx_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
  
}

topic.model.from.document.to.tbl = function(txt.file)
{
  success = FALSE
  try({
    the_txt = read_document(txt.file)
    the_txt = the_txt %>% str_squish() %>% str_replace_all(",","")
    the_txt = unlist(strsplit(the_txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    txt_this_iter = the_txt
    success = TRUE
  })
  if(success == TRUE)
  {
    cur_link = rep(txt.file, length(the_txt))
    df = tibble(cur_link, the_txt)
    colnames(df) <- c('document', 'text')
    return(df)
  } else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
  
}

topic.model.from.html.to.tbl = function(html.file)
{
  success = FALSE
  try({
    txt = xml2::read_html(html.file)
    p.txt = html_nodes(txt, "p")
    p.txt = html_text(p.txt) %>% str_squish() %>% str_replace_all(",","")
    p.txt = unlist(strsplit(p.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h1.txt = html_nodes(txt, "h1")
    h1.txt = html_text(h1.txt) %>% str_squish() %>% str_replace_all(",","")
    h1.txt = unlist(strsplit(h1.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h2.txt = html_nodes(txt, "h2")
    h2.txt = html_text(h2.txt) %>% str_squish() %>% str_replace_all(",","")
    h2.txt = unlist(strsplit(h2.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h3.txt = html_nodes(txt, "h3")
    h3.txt = html_text(h3.txt) %>% str_squish() %>% str_replace_all(",","")
    h3.txt = unlist(strsplit(h3.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h4.txt = html_nodes(txt, "h4")
    h4.txt = html_text(h4.txt) %>% str_squish() %>% str_replace_all(",","")
    h4.txt = unlist(strsplit(h4.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h5.txt = html_nodes(txt, "h5")
    h5.txt = html_text(h5.txt) %>% str_squish() %>% str_replace_all(",","")
    h5.txt = unlist(strsplit(h5.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    h6.txt = html_nodes(txt, "h6")
    h6.txt = html_text(h6.txt) %>% str_squish() %>% str_replace_all(",","")
    h6.txt = unlist(strsplit(h6.txt, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
    
    txt_this_iter = c(p.txt, h1.txt, h2.txt, h3.txt, h4.txt, h5.txt, h6.txt)
    success = TRUE
  })
  
  if(success == TRUE)
  {
    cur_link <- rep(html.file, length(txt_this_iter))
    df = tibble(cur_link, txt_this_iter)
    colnames(df) <- c('document', 'text')
    return(df)
  }
  else
  {
    df = data.frame(file=character(), text=character(), stringsAsFactors=FALSE) 
    return(df)
  }
}

