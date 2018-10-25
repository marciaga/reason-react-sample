type item = {
  title: string,
  completed: bool,
  id: int
};

type state = {
  items: list(item) /* list is the type, item is the type that the list contains */
};

/* what is pipe here? */
type action =
| AddItem(string)
| ToggleItem(int);

let str = ReasonReact.stringToElement;

let component = ReasonReact.reducerComponent("TodoApp");
/* what is this ref() call? */
let lastId = ref(0);

let valueFromEvent = (evt): string => (
  evt
  |> ReactEventRe.Form.target /* obtains the current text input from evt */
  |> ReactDOMRe.domElementToObj /* takes the DOM element from previous call and converts into "catch-all" JS obj */
)##value; /* "magic accessor syntax" to obtain the the value from the "catch-all", JS object */

let newItem = (text) => {
  /* update a ref with := */
  /* dereference a ref with ^ */
  lastId := lastId^ + 1;
  { id: lastId^, title: text, completed: false }
};

module Input = {
  type state = string;

  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) =>
      <input
        value=text
        _type="text"
        placeholder="Write something to do"
        onChange=(reduce((evt) => valueFromEvent(evt)))
        onKeyDown=((evt) =>
          if (ReactEventRe.Keyboard.key(evt) == "Enter") {
            onSubmit(text);
            (reduce(() => ""))()
          }
        )
      />
  };
};
/* use nested module */
module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
/* ~item is a labeled argument - position doesn't matter for these */
  let make = (~item, ~onToggle, children) => {
    ...component,
    render: (_self) =>
      <div
      className="item"
      onClick=((_evt) => onToggle())
      >
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(item.completed))
        />
        (str(item.title))
      </div>
  };
};

let make = _children => {
  ...component,
  initialState: () => {
    items: [
      {title: "Write things to do", completed: false, id: 0}
    ]
  },
  reducer: (action, {items}) =>
    switch action {
      | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})
      | ToggleItem(id) =>
        let items = List.map(
          (item) =>
            item.id === id ?
              {...item, completed: !item.completed} : item,
          items
        );
        ReasonReact.Update({items: items});
    },
  render: (self) => {
    let {items} = self.state;
    let numItems = List.length(items);
    <div className="app">
      <div className="title">
        (str("What to do"))
        <Input onSubmit=(text => self.send(AddItem(text))) />
      </div>
      <div className="items">
        (
          ReasonReact.arrayToElement(Array.of_list(
            List.map((item) => <TodoItem
            key=(string_of_int(item.id))
            onToggle=(_event => self.send(ToggleItem(item.id)))
            item
            />, items)
          ))
        )
      </div>
      <div className="footer">
        (str(string_of_int(numItems) ++ " items"))
      </div>
    </div>
  }
};
