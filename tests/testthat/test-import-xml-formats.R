# The readers for XML formats other than JATS, Elsevier and Wiley: each test
# writes a small made-up file in the format and reads it.

write_xml_file <- function(txt, name = "case.xml") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  f <- file.path(dir, name)
  writeLines(txt, f, useBytes = TRUE)
  f
}

body_text <- paste(rep("The participants completed the task, t(38) = 2.50, p = .017.", 3), collapse = " ")

test_that(".xml_format: the formats", {
  fmt_of <- function(txt) .xml_format(write_xml_file(txt))$format
  expect_equal(fmt_of('<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.1.2//EN" "x.dtd"><article/>'), "docbook")
  expect_equal(fmt_of('<article xmlns="http://pkp.sfu.ca"><id/></article>'), "ojs")
  expect_equal(fmt_of("<book-part-wrapper><book-meta/></book-part-wrapper>"), "bits")
  expect_equal(fmt_of('<doc sps="1.6"><doctitle>x</doctitle></doc>'), "scielo_sps")
  expect_equal(fmt_of("<TaggedPDF-doc><P>x</P></TaggedPDF-doc>"), "taggedpdf")
  expect_equal(fmt_of('<pkg:package xmlns:pkg="http://schemas.microsoft.com/office/2006/xmlPackage"/>'), "word")
  expect_equal(fmt_of('<office:document xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"/>'), "odf")
  expect_equal(fmt_of("<ArticleSet><Article/></ArticleSet>"), "pubmed")
  expect_equal(fmt_of('<doi_batch xmlns="http://www.crossref.org/schema/4.4.2"/>'), "crossref")
  expect_equal(fmt_of('<pdf2xml producer="poppler"><page/></pdf2xml>'), "pdf2xml")
  expect_equal(fmt_of("<h1>404 Not Found</h1>"), "html")
  expect_equal(fmt_of("<Error><Code>AccessDenied</Code></Error>"), "error_response")
  expect_equal(fmt_of('<br /> <b>Notice</b>: x <?xml version="1.0"?><article/>'), "junk_prefix")
  expect_equal(fmt_of("<revista><titulo>x</titulo></revista>"), "unknown")

  png <- withr::local_tempfile(fileext = ".xml")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a)), png)
  expect_equal(.xml_format(png)$format, "not_xml")
  expect_error(.read_xml_fulltext(png), "not an XML file: a png")
  pdf <- withr::local_tempfile(fileext = ".xml")
  writeLines("%PDF-1.5", pdf)
  expect_equal(.xml_format(pdf)$format, "pdf")
})

test_that("read - files that are no article stop with what they are", {
  expect_error(.read_xml_fulltext(write_xml_file("<h1>404 Not Found</h1>")), "error page")
  expect_error(.read_xml_fulltext(write_xml_file(
    "<Error><Code>AccessDenied</Code><Message>Access denied.</Message></Error>")),
    "error message from a server")
  expect_error(.read_xml_fulltext(write_xml_file(
    "<library><book><title>1984</title><author>George Orwell</author></book></library>")),
    "holds no article text")
})

test_that("read - text a server printed before the XML declaration", {
  f <- write_xml_file(paste0(
    "<br /> <b>Notice</b>: Undefined index in x.php<br />\n",
    "<?xml version=\"1.0\"?><article><front><article-meta><title-group>",
    "<article-title>A JATS article</article-title></title-group></article-meta></front>",
    "<body><p>", body_text, "</p></body></article>"), "notice.xml")
  p <- read(f)
  expect_equal(p$info$title, "A JATS article")
  expect_equal(p$info$file_name, "notice.xml")
  expect_true(all(c("t", "p") %in% p$eq$lhs))
})

test_that("read - DocBook", {
  p <- read(write_xml_file(paste0(
    '<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN" "x.dtd"><article>',
    "<articleinfo><title>A DocBook article</title><author><firstname>Ada</firstname>",
    "<surname>Byron</surname></author><abstract><para>We test DocBook.</para></abstract>",
    "<keywordset><keyword>DocBook</keyword></keywordset></articleinfo>",
    "<sect1><title>Method</title><para>", body_text,
    "<footnote><para>A note.</para></footnote></para>",
    "<itemizedlist><listitem><para>First point.</para></listitem></itemizedlist>",
    "<sect2><title>Data</title><para>The data are at <ulink url=\"https://osf.io/abcde\">OSF</ulink>.</para></sect2></sect1>",
    "<bibliography><bibliomixed>Smith, A. (2020). A study. Test Journal, 3, 1-10. https://doi.org/10.1234/tj.2020.1</bibliomixed></bibliography>",
    "</article>")))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "A DocBook article")
  expect_equal(p$author$family, "Byron")
  expect_equal(p$info$abstract, "We test DocBook.")
  expect_true(all(c("Method", "Data") %in% p$section$header))
  expect_true("First point." %in% p$text$text)
  expect_true("https://osf.io/abcde" %in% p$url$href)
  expect_equal(nrow(p$footnote), 1)
  expect_equal(p$bib$doi, "10.1234/tj.2020.1")
  expect_equal(p$extraction$producer$name, "docbook")
})

test_that("read - Word's XML: headings by style, lists, tables, footnotes", {
  p <- read(write_xml_file(paste0(
    '<pkg:package xmlns:pkg="http://schemas.microsoft.com/office/2006/xmlPackage">',
    '<pkg:part pkg:name="/word/document.xml"><pkg:xmlData>',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><w:body>',
    '<w:p><w:pPr><w:pStyle w:val="Title"/></w:pPr><w:r><w:t>A Word article</w:t></w:r></w:p>',
    '<w:p><w:pPr><w:pStyle w:val="Ttulo1"/></w:pPr><w:r><w:t>Methods</w:t></w:r></w:p>',
    '<w:p><w:r><w:t xml:space="preserve">', body_text, ' </w:t></w:r>',
    '<w:r><w:rPr><w:i/></w:rPr><w:t>Rana</w:t></w:r>',
    '<w:hyperlink r:id="rId5"><w:r><w:t>OSF</w:t></w:r></w:hyperlink></w:p>',
    '<w:p><w:pPr><w:numPr><w:ilvl w:val="0"/></w:numPr></w:pPr><w:r><w:t>First point.</w:t></w:r></w:p>',
    '<w:tbl><w:tr><w:tc><w:p><w:r><w:t>M</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>SD</w:t></w:r></w:p></w:tc></w:tr></w:tbl>',
    '</w:body></w:document></pkg:xmlData></pkg:part>',
    '<pkg:part pkg:name="/word/styles.xml"><pkg:xmlData><w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:style w:styleId="Ttulo1"><w:name w:val="heading 1"/></w:style></w:styles></pkg:xmlData></pkg:part>',
    '<pkg:part pkg:name="/word/_rels/document.xml.rels"><pkg:xmlData>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId5" Target="https://osf.io/abcde"/></Relationships></pkg:xmlData></pkg:part>',
    '</pkg:package>')))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "A Word article")
  expect_true("Methods" %in% p$section$header)
  expect_true(all(c("t", "p") %in% p$eq$lhs))
  expect_true("https://osf.io/abcde" %in% p$url$href)
  expect_true("First point." %in% p$text$text)
  expect_equal(nrow(p$table), 1)
  expect_equal(p$extraction$producer$name, "word")
})

test_that("read - OpenDocument", {
  p <- read(write_xml_file(paste0(
    '<office:document xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" ',
    'xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" ',
    'xmlns:xlink="http://www.w3.org/1999/xlink"><office:body><office:text>',
    '<text:h text:outline-level="1">An OpenDocument article</text:h>',
    '<text:h text:outline-level="2">Results</text:h>',
    '<text:p>', body_text, ' See <text:a xlink:href="https://osf.io/abcde">OSF</text:a>.',
    '<text:note><text:note-citation>1</text:note-citation><text:note-body><text:p>A note.</text:p></text:note-body></text:note></text:p>',
    '</office:text></office:body></office:document>')))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "An OpenDocument article")
  expect_true("Results" %in% p$section$header)
  expect_true("https://osf.io/abcde" %in% p$url$href)
  expect_equal(nrow(p$footnote), 1)
})

test_that("read - a web page with citation meta tags", {
  p <- read(write_xml_file(paste0(
    "<html><head><title>Page</title><meta name=\"citation_title\" content=\"A web article\"/>",
    "<meta name=\"citation_author\" content=\"Byron, Ada\"/><meta name=\"citation_doi\" content=\"10.1234/web.1\"/>",
    "<meta name=\"citation_reference\" content=\"Smith, A. (2020). A study. https://doi.org/10.1234/tj.2020.1\"/>",
    "</head><body><nav>Menu</nav><h2>Results</h2><p>", body_text, "</p>",
    "<h2>References</h2><p>Smith 2020</p></body></html>"), "page.xml"))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "A web article")
  expect_equal(p$info$doi, "10.1234/web.1")
  expect_equal(p$author$family, "Byron")
  expect_false(any(grepl("Menu", p$text$text)))
  # the references in the meta tags, not those printed on the page
  expect_equal(p$bib$doi, "10.1234/tj.2020.1")
})

test_that("read - Acrobat's tagged PDF", {
  p <- read(write_xml_file(paste0(
    "<TaggedPDF-doc><Part><H1>A tagged PDF article</H1><Sect><H2>RESUMEN</H2>",
    "<P>Probamos el formato.</P></Sect><Sect><H2>Method</H2><P>", body_text, "</P>",
    "<L><LI><Lbl>1</Lbl><LBody>First point.</LBody></LI></L></Sect>",
    "<Sect><H2>Referencias</H2><P>Smith, A. (2020). A study. https://doi.org/10.1234/tj.2020.1</P></Sect></Part></TaggedPDF-doc>")))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "A tagged PDF article")
  # the Spanish "Resumen" is the abstract, "Referencias" the references
  expect_equal(p$info$abstract, "Probamos el formato.")
  expect_equal(p$bib$doi, "10.1234/tj.2020.1")
  expect_true("First point." %in% p$text$text)
})

test_that("read - metadata records: PubMed, Crossref", {
  p <- read(write_xml_file(paste0(
    "<ArticleSet><Article><Journal><JournalTitle>Test Journal</JournalTitle><Volume>2</Volume>",
    "<PubDate><Year>2024</Year><Month>06</Month></PubDate></Journal>",
    "<ArticleTitle>A PubMed record</ArticleTitle><ELocationID EIdType=\"doi\">10.1234/pm.1</ELocationID>",
    "<AuthorList><Author><FirstName>Ada</FirstName><LastName>Byron</LastName>",
    "<Affiliation>Test University</Affiliation></Author></AuthorList>",
    "<Abstract>", body_text, "</Abstract></Article></ArticleSet>")))
  expect_equal(p$info$title, "A PubMed record")
  expect_equal(p$info$doi, "10.1234/pm.1")
  expect_equal(p$info$journal, "Test Journal")
  expect_equal(p$affiliation$text, "Test University")
  expect_match(p$info$abstract, "t\\(38\\) = 2.50")
  expect_warning(read(write_xml_file(paste0(
    "<ArticleSet><Article><ArticleTitle>One</ArticleTitle></Article>",
    "<Article><ArticleTitle>Two</ArticleTitle></Article></ArticleSet>"))), "holds 2 articles")

  p <- read(write_xml_file(paste0(
    '<doi_batch xmlns="http://www.crossref.org/schema/4.4.2" xmlns:jats="http://www.ncbi.nlm.nih.gov/JATS1">',
    "<body><journal><journal_metadata><full_title>Test Journal</full_title></journal_metadata>",
    "<journal_article><titles><title>A Crossref record</title></titles>",
    '<contributors><person_name contributor_role="author"><given_name>Ada</given_name><surname>Byron</surname></person_name></contributors>',
    "<jats:abstract><jats:p>", body_text, "</jats:p></jats:abstract>",
    "<doi_data><doi>10.1234/cr.1</doi></doi_data>",
    "<citation_list><citation><unstructured_citation>Smith, A. (2020). A study. https://doi.org/10.1234/tj.2020.1</unstructured_citation></citation></citation_list>",
    "</journal_article></journal></body></doi_batch>")))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "A Crossref record")
  expect_equal(p$info$doi, "10.1234/cr.1")
  expect_equal(p$author$family, "Byron")
  expect_equal(p$bib$doi, "10.1234/tj.2020.1")
})

test_that("read - an OJS export without a file is its metadata", {
  p <- read(write_xml_file(paste0(
    '<article xmlns="http://pkp.sfu.ca" locale="en_US"><publication locale="en_US">',
    '<id type="doi">10.1234/ojs.1</id><title locale="en_US">An OJS article</title>',
    "<abstract locale=\"en_US\">&lt;p&gt;", body_text, "&lt;/p&gt;</abstract>",
    "<keywords><keyword>OJS</keyword></keywords>",
    "<authors><author><givenname>Ada</givenname><familyname>Byron</familyname>",
    "<affiliation>Test University</affiliation></author></authors></publication></article>")))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "An OJS article")
  expect_equal(p$info$doi, "10.1234/ojs.1")
  expect_equal(unlist(p$info$keywords), "OJS")
  expect_match(p$info$abstract, "^The participants")
})

test_that("read - text with positions (pdf2xml): lines join into paragraphs", {
  p <- read(write_xml_file(paste0(
    '<pdf2xml producer="poppler"><page number="1">',
    '<fontspec id="0" size="20"/><fontspec id="1" size="10"/>',
    '<text top="100" left="50" width="300" height="20" font="0">A poppler article</text>',
    '<text top="150" left="50" width="300" height="10" font="1">The participants com-</text>',
    '<text top="162" left="50" width="300" height="10" font="1">pleted the task, t(38) = 2.50,</text>',
    '<text top="174" left="50" width="300" height="10" font="1">p = .017, as expected.</text>',
    "</page></pdf2xml>")))
  expect_equal(p$info$title, "A poppler article")
  expect_true("The participants completed the task, t(38) = 2.50, p = .017, as expected." %in% p$text$text)
})

test_that("read - a journal's own format", {
  p <- read(write_xml_file(paste0(
    "<revista><revista>Revista de Pruebas</revista><titulo>Un art\u00EDculo de prueba</titulo>",
    "<autor>Ada Byron, Ben Berg</autor><doi>https://doi.org/10.1234/own.1</doi>",
    "<resumen>Resumen: Probamos el formato propio de una revista con suficiente texto.</resumen>",
    "<texto>", body_text, "</texto><texto>", body_text, "</texto><texto>", body_text, "</texto>",
    "<referencias>Smith, A. (2020). A study. https://doi.org/10.1234/tj.2020.1</referencias></revista>")))
  expect_true(paper_validate(p))
  expect_equal(p$info$title, "Un art\u00EDculo de prueba")
  expect_equal(p$author$family, c("Byron", "Berg"))
  expect_equal(p$info$doi, "10.1234/own.1")
  expect_match(p$info$abstract, "^Probamos")
  expect_equal(p$bib$doi, "10.1234/tj.2020.1")
})
