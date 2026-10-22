-- File-contents code blocks (```{.file filename="..."}``` -- see the .file
-- rules in styles.css):
-- - Pandoc treats the first class as the language, so a block whose first
--   class is "file" has no language and Quarto renders it as a bare <pre>,
--   without a copy button or support for .numberLines. Prepend Quarto's
--   no-highlighting "default" language to turn it into a regular
--   (unhighlighted) code block. Blocks that already name a language,
--   e.g. ```{.bash .file}```, are left alone.
-- - Without a filename attribute, Quarto adds no header bar, so give it a
--   generic "file" label (styled as a label rather than a name).
function CodeBlock(el)
  if not el.classes:includes("file") then
    return nil
  end
  if el.classes[1] == "file" then
    el.classes:insert(1, "default")
  end
  if el.attributes["filename"] == nil then
    el.attributes["filename"] = "file"
  end
  return el
end
