{
  "title": "Marginal Significance",
  "description": "List all sentences that describe an effect as 'marginally significant'.",
  "authors": [{
    "orcid": "0000-0002-0247-239X",
    "name":{
      "surname": "Lakens",
      "given": "Daniël"
    },
    "email": "lakens@gmail.com"
  }],
  "text": {
    "pattern": "margin\\w* (?:\\w+\\s+){0,5}significan\\w*|trend\\w* (?:\\w+\\s+){0,1}significan\\w*|almost (?:\\w+\\s+){0,2}significan\\w*|approach\\w* (?:\\w+\\s+){0,2}significan\\w*|border\\w* (?:\\w+\\s+){0,2}significan\\w*|close to (?:\\w+\\s+){0,2}significan\\w*"
  },
  "traffic_light": {
    "found": "red",
    "not_found": "green"
  },
  "report": {
    "red": "You described effects as marginally/borderline/close to significant. It is better to write 'did not reach the threshold alpha for significance'.",
    "green": "No effects were described as marginally/borderline/close to significant."
  }
}
