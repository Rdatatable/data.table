(() => {
  const toc = document.getElementById("TOC");
  if (!toc) return;

  const content = document.querySelector(".body");
  if (!content) return;

  const headings = Array.from(
    content.querySelectorAll("h2[id], h3[id], h4[id]")
  );
  if (!headings.length) return;

  const rootList = document.createElement("ul");
  rootList.className = "nav flex-column";
  toc.appendChild(rootList);

  const listStack = [{ level: 2, list: rootList }];

  headings.forEach((heading) => {
    const level = Number(heading.tagName.slice(1));
    const text = heading.textContent.trim();
    if (!text) return;

    while (listStack.length && level < listStack[listStack.length - 1].level) {
      listStack.pop();
    }

    if (level > listStack[listStack.length - 1].level) {
      const nestedList = document.createElement("ul");
      nestedList.className = "nav flex-column ms-3";
      const parentList = listStack[listStack.length - 1].list;
      const lastItem = parentList.lastElementChild;
      if (lastItem) {
        lastItem.appendChild(nestedList);
        listStack.push({ level, list: nestedList });
      }
    }

    const currentList = listStack[listStack.length - 1].list;
    const item = document.createElement("li");
    item.className = "nav-item";

    const link = document.createElement("a");
    link.className = "nav-link px-0";
    link.href = `#${heading.id}`;
    link.textContent = text;

    item.appendChild(link);
    currentList.appendChild(item);
  });
})();
