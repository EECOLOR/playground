package documentation

class _09_Metadata {
  // Note that generated properties are generated both on POST and PATCH, 
  // we might need to change this to only POST. They can not be generated
  // using patch as we have (currently) no way of telling the API that it 
  // needs to patch more fields than are present in the current request
}