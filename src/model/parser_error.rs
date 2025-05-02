//! Error personalizado general del modelo

use std::fmt;

/// Estructura del ParserError
/// 
/// Unicamente contiene el mensaje de error
#[derive(Debug, Clone)]
pub struct ParserError {
    message: String,
}

impl fmt::Display for ParserError {
    /// Muestra el mensaje de error por pantalla
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl ParserError {
    /// Crea un ParserError y lo devuelve
    /// 
    /// # Argumentos
    /// * `message` - Mensaje del error
    /// 
    /// # Retorna
    /// A si mismo
    pub fn new(message: String) -> Self {
        ParserError {
            message
        }
    }
}