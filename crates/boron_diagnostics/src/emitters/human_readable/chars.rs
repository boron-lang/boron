#[derive(Debug, Clone)]
#[expect(dead_code)]
pub struct Characters {
  pub hbar: char,
  pub vbar: char,
  pub xbar: char,
  pub vbar_break: char,
  pub vbar_gap: char,

  pub uarrow: char,
  pub rarrow: char,

  pub ltop: char,
  pub mtop: char,
  pub rtop: char,
  pub lbot: char,
  pub rbot: char,
  pub mbot: char,

  pub lbox: char,
  pub rbox: char,

  pub lcross: char,
  pub rcross: char,

  pub underbar: char,
  pub underline: char,
  pub underline_help: char
}

pub fn ascii() -> Characters {
  Characters {
    hbar: '-',
    vbar: '|',
    xbar: '+',
    vbar_break: '*',
    vbar_gap: ':',
    uarrow: '^',
    rarrow: '>',
    ltop: ',',
    mtop: 'v',
    rtop: '.',
    lbot: '`',
    mbot: '^',
    rbot: '\'',
    lbox: '[',
    rbox: ']',
    lcross: '|',
    rcross: '|',
    underbar: '|',
    underline: '^',
    underline_help: 'ˉ'
  }
}
