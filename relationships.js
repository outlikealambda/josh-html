'use strict';

module.exports = {
  personPerson: {
    bffsWith : ':TRUSTS_EXPLICITLY',
    trusts : ':TRUSTS',
    knows : ':KNOWS',
    follows: ':TRUSTS_EXPLICITLY|:TRUSTS',
    hasRelationship: ':TRUSTS_EXPLICITLY|:TRUSTS|:KNOWS'
    constituentOf: ':CONSTITUENT_OF'
  },
  personEmail: {
    hasEmail: ':HAS_EMAIL'
  },
  personOpinion : {
    opines: ':OPINES',
    thinks: ':THINKS'
  },
  opinionTopic : {
    addresses: ':ADDRESSES'
  }
  locationRel : {
    postalCode: ':POSTAL'
    city: ':CITY'
    country: ':COUNTRY'
  }
};
