//! Error personalizado general del modelo

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeOfError {
    Error,
    Warning,
}

/// Estructura del CompilerError
///
/// Unicamente contiene el mensaje de error
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompilerError {
    message: String,
}

impl fmt::Display for CompilerError {
    /// Muestra el mensaje de error por pantalla
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl CompilerError {
    /// Crea un CompilerError y lo devuelve
    ///
    /// # Parámetros
    /// * `message` - Mensaje del error
    ///
    /// # Retorna
    /// A si mismo
    pub fn new(message: String) -> Self {
        CompilerError { message }
    }

    pub fn get_message(&self) -> String {
        self.message.clone()
    }
}
