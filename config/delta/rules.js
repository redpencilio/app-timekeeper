const rules = [
  {
    match: {
      subject: {},
    },
    callback: {
      url: 'http://resource/.mu/delta',
      method: 'POST',
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
      },
      object: {
        type: 'uri',
        value: 'http://www.w3.org/2002/12/cal/ical#Vevent'
      },
    },
    callback: {
      url: 'http://kimai-sync/delta',
      method: 'POST',
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  }
];

[
  'http://www.w3.org/2002/12/cal/ical#duration',
  'http://www.w3.org/2002/12/cal/ical#dtstart',
  'http://www.w3.org/ns/prov#wasAssociatedWith',
  'http://purl.org/dc/terms/subject',
].map((predicate) => {
  rules.push({
    match: {
      predicate: {
        type: 'uri',
        value: predicate
      },
    },
    callback: {
      url: 'http://kimai-sync/delta',
      method: 'POST',
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  });
});

export default rules;
