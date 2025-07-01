//! Módulo de la fase de identificación del analizador semántico; se utiliza el patrón Visitor
//!
//! Comprueba que todos los identificadores utilizados están definidos y que no hay duplicados.
//! También comprueba algunos tipos de identificadores

use std::collections::HashMap;

use crate::compiler_error::CompilerError;
use crate::model::{
    ast::{nodes::*, *},
    visitor::*,
};

/// enum Scope
///
/// Define los diferentes ámbitos que hay en ShExML
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
enum Scope {
    GLOBAL,
    LOCAL,
}

/// Trait que define funciones para el manejo del ámbito
trait ManageScope {
    fn get_scope(&self) -> Scope;
}

/// Enumerador que contiene todos los nodos del AST de la tabla de simbolos
// Se utiliza el enum aqui y no en el AST con el fin de tener mayor control de tipos en el AST
#[derive(Debug, PartialEq, Clone, Eq)]
enum ASTNodeSymbolTable {
    Prefix(PrefixASTNode, Scope),
    Source(SourceASTNode, Scope),
    Query(QueryASTNode, Scope),
    Iterator(IteratorASTNode, Scope),
    Field(FieldASTNode, Scope),
    Expression(ExpressionASTNode, Scope),
    Shape(ShapeASTNode, Scope),
    ShapeTuple(ShapeTupleASTNode, Scope),
}

impl ManageScope for ASTNodeSymbolTable {
    /// Devuelve el ámbito del nodo AST
    ///
    /// # Parámetros
    /// * `self` - El propio nodo del AST
    ///
    /// # Retorna
    /// El ámbito del nodo
    fn get_scope(&self) -> Scope {
        // No se puede evitar el match
        match self {
            ASTNodeSymbolTable::Prefix(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Source(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Query(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Iterator(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Field(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Expression(_, scope) => scope.clone(),
            ASTNodeSymbolTable::Shape(_, scope) => scope.clone(),
            ASTNodeSymbolTable::ShapeTuple(_, scope) => scope.clone(),
        }
    }
}

impl ManagePosition for ASTNodeSymbolTable {
    /// Devuelve la posición del nodo AST
    ///
    /// # Parámetros
    /// * `self` - El propio nodo del AST
    ///
    /// # Retorna
    /// La posición del nodo
    fn get_position(&self) -> Position {
        // No se puede evitar el match
        match self {
            ASTNodeSymbolTable::Prefix(prefix, _) => prefix.get_position(),
            ASTNodeSymbolTable::Source(source, _) => source.get_position(),
            ASTNodeSymbolTable::Query(query, _) => query.get_position(),
            ASTNodeSymbolTable::Iterator(iterator, _) => iterator.get_position(),
            ASTNodeSymbolTable::Field(field, _) => field.get_position(),
            ASTNodeSymbolTable::Expression(expression, _) => expression.get_position(),
            ASTNodeSymbolTable::Shape(shape, _) => shape.get_position(),
            ASTNodeSymbolTable::ShapeTuple(shape_tuple, _) => shape_tuple.get_position(),
        }
    }
}

/// Struct de la pila con las tablas de símbolos
struct SymbolTableStack {
    stack: Vec<HashMap<String, ASTNodeSymbolTable>>,
}

impl SymbolTableStack {
    /// Crea un nuevo stack de la tabla de símbolos
    ///
    /// # Retorna
    /// Un stack de la tabla de símbolos
    fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    /// Crea un nuevo diccionario dentro del stack; generalmente al entrar en un ámbito nuevo
    ///
    /// # Parámetros
    /// * `self` - El propio struct del stack
    fn set(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Inserta un identificador como clave y un nodo como valor en la última tabla de símbolos del stack
    ///
    /// # Parámetros
    /// * `self` - El propio struct del stack
    /// * `ident` - El identificador que se quiere insertar como clave
    /// * `node`  - El nodo que se quiere insertar como valor
    fn insert(&mut self, ident: String, node: ASTNodeSymbolTable) {
        if self.stack.last().is_none() {
            panic!("Error al acceder a la última tabla del stack");
        }
        self.stack.last_mut().unwrap().insert(ident, node);
    }

    /// Encuentra un identificador en una de las tablas de símbolos del stack, empezando por la última, y devuelve su nodo asociado
    ///
    /// # Parámetros
    /// * `self` - El propio struct del stack
    /// * `ident` - El identificador que se quiere buscar en el stack
    ///
    /// # Retorna
    /// Un Option con el nodo encontrado relacionado con el identificador pasado como parámetro
    fn find(&self, ident: String) -> Option<ASTNodeSymbolTable> {
        let mut node_finded = None;
        self.stack.clone().into_iter().rev().for_each(|table| {
            if table.contains_key(&ident) {
                node_finded = Some(table.get(&ident).unwrap().clone());
            }
        });
        node_finded
    }
}

/// Struct para poder realizar las visitas del visitor de la fase de Identificación sobre él
pub struct Identification {
    symbol_stack: SymbolTableStack,
}

impl Identification {
    /// Crea un nuevo struct Identificaction con el stack de las tablas de símbolos
    ///
    /// # Retorna
    /// Un struct de Identificación
    pub fn new() -> Self {
        Self {
            symbol_stack: SymbolTableStack::new(),
        }
    }

    /// Crea un nuevo ámbito en el stack de las tablas de símbolos
    ///
    /// # Parámetros
    /// * `self` - El propio struct de Identificación
    fn set(&mut self) {
        self.symbol_stack.set();
    }

    /// Inserta un identificador como clave y un nodo como valor en la última tabla de símbolos del stack
    ///
    /// # Parámetros
    /// * `self` - El propio struct de Identificación
    /// * `ident` - El identificador que se quiere insertar como clave
    /// * `node`  - El nodo que se quiere insertar como valor
    fn insert(&mut self, ident: String, node: ASTNodeSymbolTable) {
        self.symbol_stack.insert(ident, node);
    }

    /// Encuentra un identificador en una de las tablas de símbolos del stack, empezando por la última, y devuelve su nodo asociado
    ///
    /// # Parámetros
    /// * `self` - El propio struct de Identificación
    /// * `ident` - El identificador que se quiere buscar en el stack
    ///
    /// # Retorna
    /// Un Option con el nodo encontrado relacionado con el identificador pasado como parámetro
    fn find(&self, ident: String) -> Option<ASTNodeSymbolTable> {
        self.symbol_stack.find(ident)
    }

    /// Asocia un prefijo a una Shape o a una tupla
    ///
    /// # Parámetros
    /// * `node` - El nodo AST al que se quiere asociar el prefijo
    /// * `prefix` - El prefijo que se quiere asociar al nodo AST
    /// * `error_vec` - El vector de errores de la fase de identificación
    /// * `object_prefix` - Un booleano que indica si es el prefix del objeto o campo (true) o si no (false)
    fn asociate_prefix_to_shape<T>(
        &mut self,
        node: &mut T,
        prefix: Option<String>,
        error_vec: &mut Vec<Option<CompilerError>>,
        object_prefix: bool,
    ) where
        T: ManagePrefix + ManagePosition,
    {
        let num_line = node.get_position().get_num_line();

        if prefix.is_some() {
            let ident = prefix.unwrap();
            let node_ident = self.find(ident.clone());

            if let Some(ASTNodeSymbolTable::Prefix(prefix, _)) = node_ident {
                asociate_prefix(node, object_prefix, &prefix);
            } else {
                error_vec.push(Some(CompilerError::new(format!(
                    "Se esperaba que el identificador {ident} se correspondiera con un Prefix en la línea {num_line}"
                ))));
            }
        } else {
            // Se comprueba el prefijo por defecto (:)
            let node_ident = self.find(String::new());
            if let Some(ASTNodeSymbolTable::Prefix(prefix, _)) = node_ident {
                asociate_prefix(node, object_prefix, &prefix);
            } else if !object_prefix {
                error_vec.push(Some(CompilerError::new(format!(
                    "Se esperaba que existiera el identificador por defecto ':' en la línea {num_line}"
                ))));
            }
        }
    }

    /// Realiza el chequeo previo de un nodo
    ///
    /// Comprueba si el identificador del nodo a chequear ya existe en la tabla de símbolos
    ///
    /// # Parámetros
    /// * `error_vec` - El vecto de errores de la fase de identificación
    /// * `node` - El nodo a chequear
    ///
    /// # Retorna
    /// true si el nodo pasa el chequeo o false en caso contrario
    fn previous_checking<T>(&mut self, error_vec: &mut Vec<Option<CompilerError>>, node: &T) -> bool
    where
        T: ManagePosition + Identifiable,
    {
        let error = self.check_duplicate_identifier(node);
        if error.is_some() {
            error_vec.push(error);
            return false;
        }
        true
    }

    /// Comprueba que el identificador que se le pasa no se encuentra en la tabla de símbolos
    ///
    /// # Parámetros
    /// * `node` - El nodo cuyo identificador se quiere comprobar si está duplicado
    ///
    /// # Retorna
    /// Un Option con el posible error creado al detectar que el identificador ya existe en la tabla de símbolos
    fn check_duplicate_identifier<T>(&mut self, node: &T) -> Option<CompilerError>
    where
        T: Identifiable + ManagePosition,
    {
        let identifier = node.get_identifier();
        let node_ident = self.find(identifier.clone());

        if node_ident.is_some() {
            let duplicate: &ASTNodeSymbolTable = &node_ident.clone().unwrap();

            if node_ident.unwrap().get_scope() == duplicate.get_scope()
                && duplicate.get_scope() == Scope::GLOBAL
            {
                return create_error(identifier, node.get_position(), duplicate.get_position());
            }
        }
        None
    }
}

impl Visitor<Vec<Option<CompilerError>>> for Identification {
    /// Realiza la visita al AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `ast` - El AST que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante esta fase
    fn visit_ast(&mut self, ast: &mut AST) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        ast.get_mut_prefixes().iter_mut().for_each(|prefix| {
            if self.previous_checking(&mut error_vec, prefix) {
                error_vec.extend(self.visit_prefix(prefix));
            }
        });

        ast.get_mut_sources().iter_mut().for_each(|source| {
            if self.previous_checking(&mut error_vec, source) {
                error_vec.extend(self.visit_source(source));
            }
        });

        let queries = ast.get_mut_queries();
        if queries.is_some() {
            queries.as_mut().unwrap().iter_mut().for_each(|query| {
                if self.previous_checking(&mut error_vec, query) {
                    error_vec.extend(self.visit_query(query));
                }
            });
        }

        ast.get_mut_iterators().iter_mut().for_each(|iterator| {
            if self.previous_checking(&mut error_vec, iterator) {
                error_vec.extend(self.visit_iterator(iterator));
            }
        });

        ast.get_mut_expressions().iter_mut().for_each(|expression| {
            if self.previous_checking(&mut error_vec, expression) {
                error_vec.extend(self.visit_expression(expression));
            }
        });

        ast.get_mut_shapes().iter_mut().for_each(|shape| {
            if self.previous_checking(&mut error_vec, shape) {
                error_vec.extend(self.visit_shape(shape));
            }
        });

        error_vec
    }

    /// Realiza la visita a un nodo Prefix del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `prefix_node` - El nodo Prefix que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Prefix
    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> Vec<Option<CompilerError>> {
        if prefix_node.get_identifier() != String::new() {
            self.insert(
                prefix_node.get_identifier(),
                ASTNodeSymbolTable::Prefix(prefix_node.clone(), Scope::GLOBAL),
            );
        } else {
            // Para el prefijo por defecto (:)
            self.insert(
                String::new(),
                ASTNodeSymbolTable::Prefix(prefix_node.clone(), Scope::GLOBAL),
            );
        }
        vec![None]
    }

    /// Realiza la visita a un nodo Source del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `source_node` - El nodo Source que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Source
    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> Vec<Option<CompilerError>> {
        self.insert(
            source_node.get_identifier(),
            ASTNodeSymbolTable::Source(source_node.clone(), Scope::GLOBAL),
        );
        vec![None]
    }

    /// Realiza la visita a un nodo Query del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `query_node` - El nodo Query que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Query
    fn visit_query(&mut self, query_node: &mut QueryASTNode) -> Vec<Option<CompilerError>> {
        self.insert(
            query_node.get_identifier(),
            ASTNodeSymbolTable::Query(query_node.clone(), Scope::GLOBAL),
        );
        vec![None]
    }

    /// Realiza la visita a un nodo Iterator del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `iterator_node` - El nodo Iterator que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Iterator
    fn visit_iterator(
        &mut self,
        iterator_node: &mut IteratorASTNode,
    ) -> Vec<Option<CompilerError>> {
        self.set();
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = iterator_node.get_position().get_num_line();

        match iterator_node.get_mut_iterator_access() {
            IteratorAccess::Ident(ident) => {
                let node = self.find(ident.clone());
                if node.is_none() {
                    error_vec.push(Some(CompilerError::new(format!("No se encuentra el identificador de la Query del acceso del iterador: {}, en la línea {num_line}", ident.clone()))));
                } else {
                    if let Some(ASTNodeSymbolTable::Query(query, _)) = node {
                        iterator_node.set_query(Some(query.clone()));
                    } else {
                        error_vec.push(Some(CompilerError::new(format!("Se esperaba que el identificador {ident} se correspondiera con una consulta SQL en la línea {num_line}"))));
                    }
                }
            }
            IteratorAccess::SqlQuery(_) => (),
            IteratorAccess::CsvPerRow(_) => (),
        }

        self.insert(
            iterator_node.get_identifier(),
            ASTNodeSymbolTable::Iterator(iterator_node.clone(), Scope::GLOBAL),
        );

        iterator_node.get_mut_fields().iter_mut().for_each(|field| {
            if self.previous_checking(&mut error_vec, field) {
                error_vec.extend(self.visit_field(field));
            }
        });

        error_vec
    }

    /// Realiza la visita a un nodo Field del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `field_node` - El nodo Field que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Field
    fn visit_field(&mut self, field_node: &mut FieldASTNode) -> Vec<Option<CompilerError>> {
        self.insert(
            field_node.get_identifier(),
            ASTNodeSymbolTable::Field(field_node.clone(), Scope::LOCAL),
        );
        vec![None]
    }

    /// Realiza la visita a un nodo Expression del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `expression_node` - El nodo Expression que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Expression
    fn visit_expression(
        &mut self,
        expression_node: &mut ExpressionASTNode,
    ) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        self.insert(
            expression_node.get_identifier(),
            ASTNodeSymbolTable::Expression(expression_node.clone(), Scope::GLOBAL),
        );

        expression_node
            .get_mut_accesses()
            .iter_mut()
            .for_each(|access| {
                error_vec.extend(self.visit_access(access));
            });

        expression_node.set_fields(Some(get_expression_fields(expression_node)));

        error_vec
    }

    /// Realiza la visita a un nodo Shape del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `shape_node` - El nodo Shape que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la Shape
    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> Vec<Option<CompilerError>> {
        self.set();
        self.insert(
            shape_node.get_identifier(),
            ASTNodeSymbolTable::Shape(shape_node.clone(), Scope::GLOBAL),
        );

        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        self.asociate_prefix_to_shape(
            shape_node,
            shape_node.get_prefix_ident(),
            &mut error_vec,
            false,
        );
        self.asociate_prefix_to_shape(
            shape_node,
            shape_node.get_field_prefix_ident(),
            &mut error_vec,
            true,
        );

        shape_node.get_mut_tuples().iter_mut().for_each(|tuple| {
            if self.previous_checking(&mut error_vec, tuple) {
                error_vec.extend(self.visit_shape_tuple(tuple));
            }
        });

        match shape_node.get_mut_field_identifier() {
            IdentOrAccess::Access(access) => {
                error_vec.extend(self.visit_access(access));
            }
            IdentOrAccess::Ident(expression_ident) => {
                // No se puede extraer a una función externa debido a que shape_node es &mut
                let possible_expression = self.find(expression_ident.to_string());

                if possible_expression.is_some() {
                    if let ASTNodeSymbolTable::Expression(mut expression_node, _) =
                        possible_expression.unwrap()
                    {
                        error_vec.extend(self.visit_expression(&mut expression_node));
                        shape_node.set_expression(Some(expression_node.clone()));
                    } else {
                        error_vec.push(Some(CompilerError::new(format!("Encontrado un identificador que no se corresponde con una expresión en el campo de la Shape de la línea {}", shape_node.get_position().get_num_line()))));
                    }
                } else {
                    error_vec.push(Some(CompilerError::new(format!("Encontrado un identificador que no se corresponde con una expresión en el campo de la Shape de la línea {}", shape_node.get_position().get_num_line()))));
                }
            }
        }

        error_vec
    }

    /// Realiza la visita a un nodo tupla de la Shape del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `shape_tuple_node` - El nodo tupla de la Shape que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la tupla de la Shape
    fn visit_shape_tuple(
        &mut self,
        shape_tuple_node: &mut ShapeTupleASTNode,
    ) -> Vec<Option<CompilerError>> {
        self.insert(
            shape_tuple_node.get_identifier(),
            ASTNodeSymbolTable::ShapeTuple(shape_tuple_node.clone(), Scope::LOCAL),
        );

        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        self.asociate_prefix_to_shape(
            shape_tuple_node,
            shape_tuple_node.get_prefix_ident(),
            &mut error_vec,
            false,
        );
        self.asociate_prefix_to_shape(
            shape_tuple_node,
            shape_tuple_node.get_object_prefix_ident(),
            &mut error_vec,
            true,
        );

        match shape_tuple_node.get_mut_object() {
            IdentOrAccess::Access(access) => {
                error_vec.extend(self.visit_access(access));
            }
            IdentOrAccess::Ident(expression_ident) => {
                // No se puede extraer a una función externa debido a que shape_tuple_node es &mut
                let possible_expression = self.find(expression_ident.to_string());

                if possible_expression.is_some() {
                    if let ASTNodeSymbolTable::Expression(mut expression_node, _) =
                        possible_expression.unwrap()
                    {
                        error_vec.extend(self.visit_expression(&mut expression_node));
                        shape_tuple_node.set_expression(Some(expression_node.clone()));
                    } else {
                        error_vec.push(Some(CompilerError::new(format!("Encontrado un identificador que no se corresponde con una expresión en el campo de la Shape de la línea {}", shape_tuple_node.get_position().get_num_line()))));
                    }
                } else {
                    error_vec.push(Some(CompilerError::new(format!("Encontrado un identificador que no se corresponde con una expresión en el campo de la Shape de la línea {}", shape_tuple_node.get_position().get_num_line()))));
                }
            }
        }

        error_vec
    }

    /// Realiza la visita a un nodo Access del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `access_node` - El nodo Access que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Access
    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = access_node.get_position().get_num_line();

        let ident = access_node.get_identifier();
        let first_access = access_node.get_first_access();
        let second_access = access_node.get_second_access();

        let mut node = self.find(ident.clone());
        let mut first_access_node = self.find(first_access.clone());

        if let Some(ASTNodeSymbolTable::Source(source, _)) = &mut node {
            error_vec.extend(self.visit_source(source));
            access_node.set_source_or_expression(Some(SourceOrExpression::Source(source.clone())));
        } else if let Some(ASTNodeSymbolTable::Expression(expression, _)) = &mut node {
            error_vec.extend(self.visit_expression(expression));
            access_node
                .set_source_or_expression(Some(SourceOrExpression::Expression(expression.clone())));
        } else {
            error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Source o una Expression antes del primer '.', pero se encontró {ident} en la línea {num_line}"))));
        }

        if let Some(ASTNodeSymbolTable::Iterator(iterator, _)) = &mut first_access_node {
            error_vec.extend(self.visit_iterator(iterator));
            access_node.set_iterator(Some(iterator.clone()));
        } else if let Some(ASTNodeSymbolTable::Field(field, _)) = &mut first_access_node {
            error_vec.extend(self.visit_field(field));
            access_node.set_field(Some(field.clone()));
        } else {
            error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Iterator o Field después del primer '.', pero se encontró {first_access} en la línea {num_line}"))));
        }

        if second_access.is_some() {
            let second_access_node = self.find(second_access.clone().unwrap());
            if let Some(ASTNodeSymbolTable::Field(field, _)) = second_access_node {
                access_node.set_field(Some(field.clone()));
            } else {
                error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Field después del segundo '.', pero se encontró {} en la línea {num_line}", second_access.unwrap()))));
            }
        }

        error_vec
    }
}

/// Asocia un nodo Prefix a otro nodo
///
/// # Parámetros
/// * `node` - El nodo del AST al cual se le va a asociar el prefijo
/// * `object_prefix` - Un booleano que indica si es el prefix del objeto o campo (true) o si no (false)
/// * `prefix` - El nodo Prefix que se quiere asociar con el nodo del AST
fn asociate_prefix<T>(node: &mut T, object_prefix: bool, prefix: &PrefixASTNode)
where
    T: ManagePrefix,
{
    if !object_prefix {
        node.set_prefix(Some(prefix.clone()));
    } else {
        node.set_object_prefix(Some(prefix.clone()));
    }
}

/// Crea un error con un mensaje que indica que el identificador está duplicado
///
/// # Parámetros
/// * `ident` - El identificador duplicado
/// * `position` - La posición del nodo al que pertenece el identificador
/// * `duplicate_pos` - La posición del nodo al que pertenece el identificador que se encuentra en la tabla de símbolos
///
/// # Retorna
/// Un Option con el error que indica que hay un identificador duplicado
fn create_error(
    ident: String,
    position: Position,
    duplicate_pos: Position,
) -> Option<CompilerError> {
    Some(CompilerError::new(format!(
        "Identificador duplicado: {ident} en las líneas {} y {}",
        duplicate_pos.get_num_line(),
        position.get_num_line()
    )))
}

/// Obtiene la lista de fields que pueden ser accedidos desde una Expression
///
/// # Parámetros
/// * `expression_node` - El nodo Expression del AST al cual se quieren asociar los fields
///
/// # Retorna
/// Un vector con los nodos Field que pueden ser accedidos desde la Expression
fn get_expression_fields(expression_node: &ExpressionASTNode) -> Vec<FieldASTNode> {
    let mut expression_fields = Vec::new();

    let first_possible_iterator = expression_node
        .get_accesses()
        .get(0)
        .unwrap()
        .get_iterator();
    let first_iterator;

    if first_possible_iterator.is_none() {
        // No se muestra un error porque ya se comprueba antes que sea un iterator
        return Vec::new();
    } else {
        first_iterator = first_possible_iterator.unwrap();
    }

    let first_fields_of_iterator = first_iterator.get_fields();

    if expression_node.get_accesses().len() >= 2 {
        let second_iterator = expression_node
            .get_accesses()
            .get(1)
            .unwrap()
            .get_iterator()
            .unwrap();
        let second_fields_of_iterator = second_iterator.get_fields();

        let mut first_fields = Vec::new();
        let mut second_fields = Vec::new();

        first_fields_of_iterator.into_iter().for_each(|field| {
            first_fields.push(field);
        });

        second_fields_of_iterator.into_iter().for_each(|field| {
            second_fields.push(field);
        });

        expression_fields.extend(get_fields_intersection(first_fields, second_fields));
    } else {
        first_fields_of_iterator.into_iter().for_each(|field| {
            expression_fields.push(field);
        });
    }

    expression_fields
}

/// Obtiene la intersección de 2 vectores de nodos Field, es decir, los nodos Field que se encuentran en ambos vectores
///
/// # Parámetros
/// * `vec1` - El primer vector de nodos Field
/// * `vec2` - El segundo vector de nodos Field
///
/// # Retorna
/// Un vector de nodos Field que contiene la intersección de los 2 vectores pasados como parámetros
fn get_fields_intersection(vec1: Vec<FieldASTNode>, vec2: Vec<FieldASTNode>) -> Vec<FieldASTNode> {
    let mut vec = Vec::new();
    vec.extend(vec1.clone());

    for field in vec.clone() {
        if !vec2.contains(&field) {
            if let Some(index) = vec
                .iter()
                .position(|x| x.get_identifier() == field.get_identifier())
            {
                vec.remove(index);
            }
        }
    }

    vec
}
