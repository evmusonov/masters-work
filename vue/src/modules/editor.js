import { EditorState, EditorView, basicSetup } from "@codemirror/basic-setup";

let theme = EditorView.theme(
  {
    "&": {
      color: "white",
      backgroundColor: "#034",
    },
    ".cm-content": {
      caretColor: "#0e9",
    },
    "&.cm-focused .cm-cursor": {
      borderLeftColor: "#0e9",
    },
    "&.cm-focused .cm-selectionBackground, ::selection": {
      backgroundColor: "#074",
    },
    ".cm-gutters": {
      backgroundColor: "#045",
      color: "#ddd",
      border: "none",
    },
    ".cm-scroller": {
      height: "300px",
    },
  },
  { dark: true }
);

export default (element) => {
  let view = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, theme],
    }),
    contentHeight: 100,
    parent: element,
  });

  return view;
}